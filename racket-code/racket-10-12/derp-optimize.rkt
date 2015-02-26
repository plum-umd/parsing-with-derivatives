(module derp-optimize
  racket
  
  (provide (all-defined-out))
  
  (require "derp-core.rkt")
  (require "memoization.rkt")
  (require "fixed-points.rkt")
  
  ; determine if the language accepts ε
  (define/fix (nullable? L)
    #:bottom #f
    (match L
      [(∅)           #f]
      [(ε _)         #t]
      [(δ p)         (nullable? p)]
      [(token _)     #f]
      [(∪ L1 L2)     (or (nullable? L1) (nullable? L2))]
      [(∘ L1 L2)     (and (nullable? L1) (nullable? L2))]
      [(★ L1)        #t]
      [(→ L1 _)      (nullable? L1)]))
    
  
  ; determine if the language is "essentially" ε
  (define/fix (essentially-ε? L)
    #:bottom #f
    (match L
      [(∅)            #f]
      [(ε _)          #t]
      [(δ p)          (nullable? p)]
      [(token _)      #f]
      [(∪ L1 L2)      (and (essentially-ε? L1) (essentially-ε? L2))]
      [(∘ L1 L2)      (and (essentially-ε? L1) (essentially-ε? L2))]
      [(★ L1)         (or  (essentially-ε? L1) (essentially-∅? L1))]
      [(→ L1 _)       (essentially-ε? L1)]))

  ; retrieves the element of a singleton set
  (define (get-singleton s)
    (define el #f)
    (for ([el* s])
      (set! el el*))
    el)
  
  ; Matches a language if it is essentially-ε? and carries a parse-null set with size 1.
  ; Binds the singleton element in the set to the argument pattern.
  (define-match-expander singleton-ε?
    (syntax-rules ()
      [(_)    (? essentially-ε?)]
      [(_ el) (and (? essentially-ε?) 
                   (app parse-null (and (app set-count 1) 
                                        (app get-singleton el))))]))
  
  ; determine if the language is "essentailly" ∅
  (define/fix (essentially-∅? L)
    #:bottom #t
    (match L
      [(∅)         #t]
      [(ε s)       (set-empty? s)]
      [(δ p)       (not (nullable? p))]
      [(token _)   #f]
      [(★ L1)      #f]
      [(∪ L1 L2)   (and (essentially-∅? L1) (essentially-∅? L2))]
      [(∘ L1 L2)   (or  (essentially-∅? L1) (essentially-∅? L2))]
      [(→ L1 _)    (essentially-∅?  L1)]))
 
  ; Optimizing compaction.
  ; (K L) is an equivalent, compacted version of L.
  (define/memoize (K [L #:eq])
    (match L
      [(∅)                       L]      
      [(ε _)                     L]
      
      [(? essentially-∅?)        (∅)]
      [(? essentially-ε?)        (ε (parse-null L))]
      
      ; this case is handled by either essentially-∅ or essentially-ε
      ; [(δ _) ...]
      
      [(token _)                  L]
      
      [(∪ (? essentially-∅?) L2) (K L2)]
      [(∪ L1 (? essentially-∅?)) (K L1)]
      [(∪ L1 L2)                 (∪ (K L1) (K L2))]
      
      [(∘ (singleton-ε? e) L2)   (→ (K L2) (λ (w2) (cons e w2)))]
      [(∘ L1 (singleton-ε? e))   (→ (K L1) (λ (w1) (cons w1 e)))]
      [(∘ L1 L2)                 (∘ (K L1) (K L2))]
      
      [(★ (? essentially-∅?))    (ε (set '()))]
      [(★ L)                     (★ (K L))]      
      
      [(→ (→ L f) g)             (→ (K L) (compose g f))]
      [(→ L f)                   (→ (K L) f)]))

  ; Parsing with interleaved compaction:
  (define (parse/compact w L #:compactor [compact K])
    (if (null? w)
        (parse-null L)
        (parse/compact (cdr w) (compact (D (car w) L)))))
  
  ; The nth derivative with compaction:
  (define (D/compact/n n w L #:compactor [compact K])
    (if (equal? n 0)
        L
        (if (null? w)
            L
            (D/compact/n (- n 1) (cdr w) (compact (D (car w) L)))))))
