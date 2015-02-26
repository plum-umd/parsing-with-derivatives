(module lazy-structs
  racket/base
  
  (require (for-syntax racket/base))
  (require racket/match)
  (require (for-syntax racket/match))
  (require racket/promise)
  
  (provide define-lazy-struct)
  
  (define-syntax (define-lazy-struct stx)
    (syntax-case stx ()
      [(_ name {field ...})
       (with-syntax ([$name (datum->syntax #'name (gensym (syntax->datum #'name)))])
         #'(begin
             
             (define-struct $name {field ...})
             
             (define-match-expander name 
               (syntax-rules () [(_ field ...)
                                 ; =>
                                 ($name (app force field) ...)])
               
               (syntax-rules () [(_ field ...)
                                 ; =>
                                 ($name (delay field) ...)]))))])))
         