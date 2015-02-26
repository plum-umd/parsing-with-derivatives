(module derp-core
  racket
  
  (require "memoization.rkt")
  (require "fixed-points.rkt")
  (require "lazy-structs.rkt")

  (provide (all-defined-out))
  
  ; Atomic parsers:
  (define-struct ∅       {})         ; the empty language
  (define-struct ε       {tree-set}) ; the empty-string language
  (define-struct token   {value?})   ; either a class of tokens defined by a predicate or a singleton language
  
  ; Compound parsers:
  (define-lazy-struct δ {lang})        ; nullifies a language
  (define-lazy-struct ∪ {this that})   ; union of two languages
  (define-lazy-struct ∘ {left right})  ; sequence of two languages
  (define-lazy-struct ★ {lang})        ; Kleene star repetition of a language
  (define-lazy-struct → {lang reduce}) ; functional reduction of a language
  
  ; Derivative:
  (define/memoize (D c p)
    #:order ([p #:eq] [c #:equal])
    (match p
      [(∅)           (∅)]
      [(ε _)         (∅)]
      [(δ _)         (∅)]
      [(token t)      (if (procedure? t)
                          (if (t c) 
                              (ε (set c))
                              (∅))
                          (if (equal? t c)
                              (ε (set c))
                              (∅)))]
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
      [(∅)          (set)]
      [(ε S)        S]
      [(δ p)        (parse-null p)]
      [(token _)    (set)]
      
      [(★ _)        (set '())]
      [(∪ p1 p2)    (set-union (parse-null p1) (parse-null p2))]
      [(∘ p1 p2)    (for*/set ([t1 (parse-null p1)]
                               [t2 (parse-null p2)])
                              (cons t1 t2))]
      [(→ p1 f)     (for/set ([t (parse-null p1)])
                             (f t))]))
  
  ; Parsing:
  (define (parse w p)
    (if (null? w)
        (parse-null p)
        (parse (cdr w) (D (car w) p))))
  
  ; The nth derivative:
  (define (D/n n w p)
    (if (equal? n 0)
        p
        (if (null? w)
            p
            (D/n (- n 1) (cdr w) (D (car w) p))))))
