(module derp-sugar
  racket
  
  (provide (all-defined-out))
  
  (require "derp-core.rkt")
  
  (define-syntax (lang stx)
    (syntax-case stx (∅ ε ε* quote token? 
                        empty eps eps*
                        ∪ ★ + ? ∘ 
                        or rep rep+ opt seq
                        list list! unquote
                        quasiquote
                        → --> $--> @--> >--> car)
      [(f L)           (with-syntax ([literal->language 
                                      (datum->syntax #'L 'literal->language)])
                         #'(lang literal->language L))]
      [(_ ll (∅))             #'(∅)]
      [(_ ll (ε))             #'(ε (set '()))]
      [(_ ll (ε v))           #'(ε (set v))]
      [(_ ll (ε* S))          #'(ε S)]
      [(_ ll (token? pred))   #'(token? pred)]
      [(f ll (quote lit))     #'(ll 'lit)]
      
      [(f ll (empty))         #'(f ll (∅))]
      [(f ll (eps v))         #'(f ll (ε v))]
      [(f ll (eps* S))        #'(f ll (ε* S))]
      
      [(_ ll (∪))              #'(∅)]
      [(f ll (∪ l1))           #'(f ll l1)]
      [(f ll (∪ l1 l2 ...))    #'(∪ (f ll l1) (f ll (∪ l2 ...)))]
      [(f ll (or l1 ...))      #'(f ll (∪ l1 ...))]
      
      [(_ ll (∘))              #'(ε (set #f))]
      [(f ll (∘ l1))           #'(f ll l1)]
      [(f ll (∘ l1 l2 ...))    #'(∘ (f ll l1) (f ll (∘ l2 ...)))]
      [(f ll (seq l1 ...))      #'(f ll (∘ l1 ...))]

      
      [(_ ll (list))           #'(ε (set '()))]
      [(f ll (list l1))        #'(→ (f ll l1) (λ (w1) (list w1)))]
      [(f ll (list l1 l2 ...)) #'(∘ (f ll l1) (f ll (list l2 ...)))]
      
      [(_ ll (list!))            #'(ε (set '()))]
      [(f ll (list! ,l1 l2 ...)) #'(f ll (∘ (f ll l1) (list! l2 ...)))]
      [(f ll (list! l1 l2 ...))  #'(f ll (--> (∘ (f ll l1) (list! l2 ...)) cdr))]
      
      [(f ll `())               #'(ε (set '()))]
      [(f ll `(,hd tl ...))     #'(∘ (f ll hd) (f ll `(tl ...)))]
      [(f ll `(hd tl ...))      #'(∘ (f ll 'hd) (f ll `(tl ...)))]
      
      [(f ll (★ l))             #'(★ (f ll l))]
      [(f ll (rep l))           #'(f ll (★ l))]
      
      [(f ll (+ l))             #'(∘ (f ll l) (★ (f ll l)))]
      [(f ll (rep+ l))          #'(f ll (+ l))]
      
      [(f ll (? l))             #'(∪ (f ll l) (ε* (set #f)))]
      [(f ll (? l v))           #'(∪ (f ll l) (ε* (set v)))]
      [(f ll (opt x ...))       #'(f ll (? x ...))]
      
      [(f ll (car l))           #'(→ (f ll l) car)]
      
      
      [(f ll (→ l g))           #'(→ (f ll l) g)]
      [(f ll (-->  l g))        #'(→ (f ll l) g)]
      [(f ll (@--> l g))        #'(→ (f ll l) (λ (w) (apply g w)))]
      [(f ll (>--> l c ...))    #'(→ (f ll l) (λ (w) (match w c ...)))]
      [(f ll ($--> l e ...))    (with-syntax ([$  (datum->syntax #'l '$)]
                                              [$$ (datum->syntax #'l '$$)])
                                  #'(→ (f ll l)
                                       (λ ($$)
                                         (let (($ (λ (n) (list-ref $$ n))))
                                           e ...))))]
      
      [(f ll atom)            (let ((d (syntax->datum #'atom)))
                                (cond
                                  [(string? d)   #'(ll atom)]
                                  [(number? d)   #'(ll atom)]
                                  [(boolean? d)  #'(ll atom)]
                                  [else          #'atom]))]
      
      [else                 (error "syntax error in lang")]))
  
  ; Specifies the default behavior for literals in the grammar:
  (define (default-literal->language lit)
    (token (λ (t) (equal? t lit))))
  
  (define literal->language default-literal->language)
  
  ; Set the behavior for literals in the grammar:
  (define (set-literal->language! f)
    (set! literal->language f))
  
  ; Tools for defining grammars:
  (define-syntax grammar-rule
    (syntax-rules ()
      [(_ #:literals ll (lhs (rhs ...)))
       (define lhs (lang ll (rhs ...)))]
            
      [(_ #:literals ll (lhs rhs))
       (define lhs (lang ll (--> rhs (λ (x) x))))]
      
      [(_ (lhs (rhs ...)))
       (define lhs (lang (rhs ...)))]
      
      [(_ (lhs rhs))
       (define lhs (lang (--> rhs (λ (x) x))))]))
          
  
  (define-syntax grammar
    (syntax-rules ()
      [(_)   (∅)]
      
      [(_ #:start body rules ...)
       ; =>
       (grammar rules ... body)]
      
      [(_ #:literals ll (lhs rhs) ... body)
       ; =>
       (let ()
         (grammar-rule #:literals ll (lhs rhs)) ... 
         body)]
  
      [(_ (lhs rhs) ... body)
       ; =>
       (let ()
         (grammar-rule (lhs rhs)) ... 
         body)]))
  
  (define-syntax define-grammar
    (syntax-rules ()
      [(_ name rest ...)
       (define name (grammar rest ...))])))
