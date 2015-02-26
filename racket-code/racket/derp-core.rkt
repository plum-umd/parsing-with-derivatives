(module derp-core
  racket
  
  (require "memoization.rkt")
  (require "fixed-points.rkt")
  (require "lazy-structs.rkt")

  (provide (all-defined-out))
  
  ; Atomic parsers:
  (define-struct ∅      {})           ; empty set
  (define-struct ε      {tree-set})   ; empty string
  (define-struct token  {value?})      ; token class
  
  ; Compound parsers:
  (define-lazy-struct δ {lang})
  (define-lazy-struct ∪ {this that})
  (define-lazy-struct ∘ {left right})
  (define-lazy-struct ★ {lang})
  (define-lazy-struct → {lang reduce})
  
  ; Derivative:
  (define/memoize (D c p)
    #:order ([p #:eq] [c #:equal])
    (match p
      [(∅)           (∅)]
      [(ε _)         (∅)]
      [(δ _)         (∅)]
      [(token p?)    (cond
                       [(p? c) (ε (set c))]
                       [else   (∅)])]
      
      [(∪ p1 p2)     (∪ (D c p1) 
                        (D c p2))]
      [(★ p1)        (∘ (D c p1) p)]
      [(→ p1 f)      (→ (D c p1) f)]
      [(∘ p1 p2)     (∪ (∘ (δ p1) (D c p2))
                        (∘ (D c p1) p2))]))
  
  ; Parsing null:
  (define/fix (parse-null p)
    #:bottom (set)
    (match p
      [(ε S)        S]
      [(∅)          (set)]
      [(δ p)        (parse-null p)]
      [(token _)    (set)]
      
      [(★ _)        (set '())]
      [(∪ p1 p2)    (set-union (parse-null p1) (parse-null p2))]
      [(∘ p1 p2)    (for*/set ([t1 (parse-null p1)]
                               [t2 (parse-null p2)])
                              (cons t1 t2))]
      [(→ p1 f)     (for/set ([t (parse-null p1)])
                             (f t))]))
  
  ; Parse a list of tokens:
  (define (parse w p)
    (if (null? w)
        (parse-null p)
        (parse (cdr w) (D (car w) p)))))
