(module derp-optimize
  racket
  
  (provide (all-defined-out))
  
  (require (except-in "derp-core.rkt" ∅? ε?))
  (require "memoization.rkt")
  (require "fixed-points.rkt")
  
  (define/fix (ε? L)
    #:bottom #t
    (match L
      [(∅)           #f]
      [(ε _)         #t]    
      [(token _)     #f]
      [(∪ L1 L2)     (and (ε? L1) (ε? L2))]
      [(∘ L1 L2)     (and (ε? L1) (ε? L2))]
      [(★ L1)        (or  (ε? L1) (∅? L1))]
      [(→ L1 _)      (ε?  L1)]))
  
  ; Compute the size of a set:
  (define (set-choose s)
    (define el #f)
    (for ([el* s])
      (set! el el*))
    el)
  
  ; Matches a language if it is *exactly* the empty string:
  (define-match-expander nullp
    (syntax-rules ()
      [(_)    (? ε?)]
      [(_ el) (and (? ε?) 
                   (app parse-null (and (app set-count 1) 
                                        (app set-choose el))))]))
  
  ; Checks whether a language is the empty set:
  (define/fix (∅? L)
    #:bottom #f
    (match L
      [(∅)         #t]
      [(ε _)       #f]    
      [(token _)   #f]
      [(★ L1)      #f]
      [(∪ L1 L2)   (and (∅? L1) (∅? L2))]
      [(∘ L1 L2)   (or  (∅? L1) (∅? L2))]
      [(→ L1 _)    (∅?  L1)]))
 
  ; Optimizing compaction.
  ; (K L) is an equivalent, compacted version of L.
  (define/memoize (K [L #:eq])
    (match L
      [(∅)         L]
      [(ε _)       L]
      [(? ∅?)      (∅)]
      [(? ε?)      (ε (parse-null L))]
      [(token _)   L]
      
      [(★ (? ∅?))    (ε (set '()))]
      [(★ L)         (★ (K L))]
      
      [(∪ (? ∅?) L2)   (K L2)]
      [(∪ L1 (? ∅?))   (K L1)]
      
      [(∘ (nullp t) L2)   (→ (K L2) (λ (w2) (cons t w2)))]
      [(∘ L1 (nullp t))   (→ (K L1) (λ (w1) (cons w1 t)))]
      
      [(∪ L1 L2)   (∪ (K L1) (K L2))]
      [(∘ L1 L2)   (∘ (K L1) (K L2))]
      
      [(→ (and e (? ε?)) f) 
       (ε (for/set ([t (parse-null e)]) (f t)))]
      
      [(→ (∘ (nullp t) L2) f)   (→ (K L2) (λ (w2) (f (cons t w2))))]
      [(→ (→ L f) g)            (→ (K L) (compose g f))]
      [(→ L f)                  (→ (K L) f)]))

  (define (parse/compact w L #:compactor [compact K])
    (if (null? w)
        (parse-null L)
        (parse/compact (cdr w) (compact L)))))
    
  
