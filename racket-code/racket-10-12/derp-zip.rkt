(module derp-zip
  racket
  
  (provide (all-defined-out))
  
  (require "derp-core.rkt")
  (require "derp-optimize.rkt")
  (require "lazy-structs.rkt")
  (require "memoization.rkt")
  (require "fixed-points.rkt")
  
  ; A zipper parser: a parser and a context
  (define-struct zipper {parser context})
  
  ; The context for zipper parsers
  ; the top context is represented by 'top
  (define-struct ☐∘✹ {L2 k})     ; sequence with a missing left parser
  (define-struct ✹∘☐ {L1 k})     ; sequence with a missing right parser
  (define-struct ☐→✹ {L k})      ; reduction with a missing parser
  
  ; Fills the hole of a zipper with a parser
  (define (pzip Z)
    (match Z
      [(zipper L 'top)         L]

      [(zipper L1 (☐∘✹ L2 k))  (pzip (zipper (∘ L1 L2) k))]
      [(zipper L2 (✹∘☐ L1 k))  (pzip (zipper (∘ L1 L2) k))]
      [(zipper L (☐→✹ f k))    (pzip (zipper (→ L f) k))]))
  
  ; Drills into a parser and focuses on the next relavent piece
  (define (drill Z)
    (match Z
      [(zipper L k)
       (match L
         [(∘ (? essentially-ε? L1) (and (not (? essentially-ε?)) L2))        (drill (zipper L2 (✹∘☐ L1 k)))]
         [(∘ (and L1 (not (? nullable?))) L2) (drill (zipper L1 (☐∘✹ L2 k)))]
         [(→ (and (not (? essentially-ε?)) L) f)                             (drill (zipper L (☐→✹ f k)))]
         [_                                   Z])]))

  ; Backs out of a focuses context in the event the focuses parser is nullable
  (define (regress Z)
    (match Z
      [(zipper (? nullable? L1) (☐∘✹ L2 k))   (regress (zipper (∘ L1 L2) k))]
      [(zipper (? nullable? L2) (✹∘☐ L1 k))   (regress (zipper (∘ L1 L2) k))]
      [(zipper (? nullable? L1) (☐→✹ f k))    (regress (zipper (→ L1 f) k))]
      [(zipper L k)                           (zipper (K L) k)]))
  
  ; Parsing with interleaved compaction and focusing
  (define (parse/zip w L #:compactor [compact K])
    (parse/zipped w (zipper L 'top) #:compactor compact))
  
  (define (parse/zipped w Z #:compactor [compact K])
    (if (null? w)
        (parse-null (pzip Z))
        (parse/zipped 
         (cdr w)
         (match Z
           [(zipper L k) 
           (drill
            (regress 
             (zipper 
              (compact (D (car w) L)) 
              k)))]))))
  
  ; The nth derivative with compaction and focusing
  (define (D/zip/n n w L #:compactor [compact K])
    (D/zipped/n n w (zipper L 'top) #:compactor compact))
  
  (define (D/zipped/n n w Z #:compactor [compact K])
    (if (equal? n 0)
        Z
        (if (null? w)
            Z
            (D/zipped/n 
             (- n 1) 
             (cdr w)
             (match Z
               [(zipper L k) 
               (drill
                (regress 
                 (zipper 
                  (compact (D (car w) L)) 
                  k)))]))))))
