#lang racket

; Author: Matthew Might
;   Site: http://matt.might.net/

; This file shows how to parse with derivatives.

; See the bottom for test cases.

(require srfi/41) ; Stream library
(require test-engine/racket-tests)

; Generic tools:
(define-syntax while
  (syntax-rules ()
    [(_ cond body ...)
     ; =>
     (letrec ((lp (lambda () (when cond body ... (lp)))))
       (lp))]))

(define-syntax define/fix
  (syntax-rules ()
    [(_ (f x) #:bottom bottom body ...)
     ; =>
     (define f (let ((cache     (make-weak-hasheq))
                     (changed?  (make-parameter 'error-changed))
                     (running?  (make-parameter #f))
                     (visited   (make-parameter 'error-visited)))
                 (lambda (x)
                   (let ((cached? (hash-has-key? cache x))
                         (cached  (hash-ref cache x (lambda () bottom)))
                         (run?    (running?)))
                     (cond
                       [(and cached? (not run?))
                        ; =>
                        cached]
                       
                       [(and run? (hash-has-key? (unbox (visited)) x))
                        ; =>
                        (if cached? cached bottom)]
                       
                       [run? 
                        ; =>
                        (hash-set! (unbox (visited)) x #t)
                        (let ((new-val (begin body ...)))
                          (when (not (equal? new-val cached))
                            (set-box! (changed?) #t)
                            (hash-set! cache x new-val))
                          new-val)]
                       
                       [(and (not cached?) (not run?))
                        ; =>
                        (parameterize ([changed? (box #t)]
                                       [running? #t]
                                       [visited (box (make-weak-hasheq))])
                          (let ([v bottom])
                            (while (unbox (changed?))
                                   (set-box! (changed?) #f)
                                   (set-box! (visited) (make-weak-hasheq))
                                   (set! v (f x)))
                            v))])))))]))

(define-syntax make-weak-hash-trie
  (syntax-rules ()
    [(_ #:eq eq ...)      (make-weak-hasheq)]
    [(_ #:eqv eq ...)     (make-weak-hasheqv)]
    [(_ #:equal eq ...)   (make-weak-hash)]))

(define-syntax weak-hash-trie-get!
  (syntax-rules ()
    [(_ t [eq] [x] lazy-val)
     ; =>
     (let ([$t t]
           [$x x])
       (if (hash-has-key? $t $x)
           (hash-ref $t $x)
           (let ([val lazy-val])
             (hash-set! $t $x val)
             val)))]
    
    [(_ t [eq1 eq2 eq3 ...] [x1 x2 x3 ...] lazy-val)
     ; =>
     (let ([$t t])
       (if (hash-has-key? t x1)
           (let ([t2 (hash-ref t x1)])
             (weak-hash-trie-get! t2 [eq2 eq3 ...] [x2 x3 ...] lazy-val))
           (let ([t2 (make-weak-hash-trie eq2 eq3 ...)])
             (hash-set! t x1 t2)
             (weak-hash-trie-get! t2 [eq2 eq3 ...] [x2 x3 ...] lazy-val))))]))
 
(define-syntax define/memoize 
  (syntax-rules ()
    [(_ (f [v eq] ...) body ...)
     ; =>
     (define/memoize (f v ...) #:order ([v eq] ...) body ...)]
    
    [(_ (f v ...) #:order ([v* eq] ...) body ...)
     ; =>
     (define f (let ((cache (make-weak-hash-trie eq ...))
                     ($f    (lambda (v ...) (let ([v* v] ...) body ...))))
                 (lambda (v ...)
                   (let ([v* v] ...)
                     (weak-hash-trie-get! cache [eq ...] [v ...] ($f v ...))))))]
    
    [(_ (f v ...) body ...)
     ; =>
     (define/memoize (f [v #:equal] ...) body ...)]))
    
; Languages:
(define-struct language ())

; Atomic languages:
(define-struct (empty language) ())           ; empty set
(define-struct (eps   language) ())           ; empty string
(define-struct (token language) (pred class)) ; terminal

; Compound languages:
(define-struct (compound-language language)      ())
(define-struct (union         compound-language) (this that))
(define-struct (concatenation compound-language) (left right))
(define-struct (reduction     compound-language) (lang reduce))
(define-struct (repetition    compound-language) (lang))

; Special symbols that can only appear during parsing:
(define-struct (eps*  eps)  (tree-set)) ; empty string that produces a tree

; Constructors:
(define-syntax cat
  (syntax-rules ()
    [(_)            (eps)]
    [(_ l1)         l1]
    [(_ l1 l2 ...)  (concatenation (delay l1) (delay (cat l2 ...)))]))

(define-syntax alt
  (syntax-rules ()
    [(_)            (empty)]
    [(_ l1)         l1]
    [(_ l1 l2 ...)  (union (delay l1) (delay (alt l2 ...)))]))

(define-syntax red
  (syntax-rules ()
    [(_ l f)        (reduction (delay l) f)]
    [(_ l)          (reduction (delay l) (lambda (x) x))]))

(define-syntax rep
  (syntax-rules ()
    [(_ l)        (repetition (delay l))]))

(define-syntax lang
  (syntax-rules (or quote rep seq --> empty eps ?)
    [(_)                  (empty)]
    [(_ (eps))            (eps)]
    [(_ (empty))          (empty)]
    [(_ (? pred class))   (token pred class)]
    [(_ (quote lit))      (token (lambda (t) (equal? t 'lit)) 'lit)]
    
    [(_ (or))             (empty)]
    [(_ (or l1))          (lang l1)]
    [(_ (or l1 l2 ...))   (alt (lang l1) (lang (or l2 ...)))]
    
    [(_ (seq))            (eps)]
    [(_ (seq l1))         (lang l1)]
    [(_ (seq l1 l2 ...))  (cat (lang l1) (lang (seq l2 ...)))]
    
    [(_ (rep l))          (rep l)]
    
    [(_ (--> l f))        (red (lang l) f)]
                
    [(_ var)              var]))
    
; Pattern-matchers on languages:
(define-match-expander orp
  (syntax-rules ()
    [(_ l1 l2) (union (app force l1) (app force l2))]))

(define-match-expander seqp
  (syntax-rules ()
    [(_ l1 l2) (concatenation (app force l1) (app force l2))]))

(define-match-expander redp
  (syntax-rules ()
    [(_ l f) (reduction (app force l) f)]))

(define-match-expander repp
  (syntax-rules ()
    [(_ l) (repetition (app force l))]))

(define-match-expander nullablep
  (syntax-rules ()
    [(_) (app nullable? #t)]))

; Parse a stream into a tree:    
(define (parse l s 
               #:compact   [compact   (lambda (x) x)] 
               #:steps     [n #f] 
               #:debug     [debug? #f])
  (cond
    [(and n (= n 0))  l]
    [(stream-null? s) (parse-null l)]
    [else             
     ; =>
     (let* ([c      (stream-car s)]
            [rest   (stream-cdr s)]
            [dl/dc  (parse-derive c l)]
            [l*     (compact dl/dc)])
       (when debug?
         (display (format "size: ~s; mem: ~s~n" (language-size l*) (current-memory-use))))
       (parse l* rest
              #:compact  compact
              #:steps    (and n (- n 1))
              #:debug    debug?))]))

(define (parse-null/input l input)
  (list->stream (set-map (parse-null l) (lambda (el) (cons el input)))))

; Nullability:
(define/fix (nullable? l)
  #:bottom #f
  (match l
    [(empty)           #f]
    [(eps)             #t]    
    [(token _ _)       #f]
    [(repp _)          #t]
    [(orp l1 l2)       (or (nullable? l1) (nullable? l2))]
    [(seqp l1 l2)      (and (nullable? l1) (nullable? l2))]
    [(redp l1 _)       (nullable? l1)]))

; Parse trees for nullability:
(define empty-tree-set (set))

(define/fix (parse-null l)
  #:bottom empty-tree-set
  (match l
    [(empty)        empty-tree-set]
    [(eps* S)       S]
    [(eps)          (set l)]
    [(token _ _)    empty-tree-set]
    [(repp (nullp)) (error "infinite parse-null")]
    [(repp _)       (set '())]
    [(orp  l1 l2)   (set-union (parse-null l1) (parse-null l2))]
    [(seqp l1 l2)   (for*/set ([t1 (parse-null l1)]
                               [t2 (parse-null l2)])
                              (cons t1 t2))]
    [(redp l1 f)    (for/set ([t (parse-null l1)])
                             (f t))]))

; Derivative of a parser combinator:
(define/memoize (parse-derive c l)
  #:order ([l #:eq] [c #:equal])
  
;  (define l* (simplify l))
  (define l* l)
  
  (match l*
    [(empty)     l*]
    [(eps)       (empty)]
    [(token pred class)
     ; =>
     (if (pred c) (eps* (set c)) (empty))]
    
    [(orp l1 l2)
     ; =>
     (alt (parse-derive c l1) 
          (parse-derive c l2))]
    
    [(seqp (and (nullp) l1) l2)
     ; =>
     (cat (eps* (parse-null l1)) (parse-derive c l2))]
    
    [(seqp (and (nullablep) l1) l2)
     ; =>
     (alt (cat (eps* (parse-null l1)) (parse-derive c l2))
          (cat (parse-derive c l1) l2))]
    
    [(seqp l1 l2)
     ; =>
     (cat (parse-derive c l1) l2)]
    
    [(repp l1)
     ; =>
     (cat (parse-derive c l1) l)]
    
    [(redp l f)
     ; =>
     (red (parse-derive c l) f)]))

; Derivative of a context-free language:
(define/memoize (derive c l)
  #:order ([l #:eq] [c #:equal])
  (match l
    [(empty)
     ; =>
     l]
    
    [(eps)
     ; =>
     (empty)]
    
    [(token pred class)
     ; =>
     (if (pred c) (eps) (empty))]
    
    [(orp l1 l2)
     ; =>
     (alt (derive c l1) 
          (derive c l2))]
    
    [(seqp (and (nullablep) l1) l2)
     ; =>
     (alt (derive c l2)
          (cat (derive c l1) l2))]
    
    [(seqp l1 l2)
     ; =>
     (cat (derive c l1) l2)]
    
    [(repp l1)
     ; =>
     (cat (derive c l1) l)]
    
    [(redp l f)
     ; =>
     (derive c l)]))

; Recognizes if a string is in a language:
(define (recognizes? l s #:compact [compact (lambda (x) x)] #:debug [debug? #f])
  (cond
    [(stream-null? s) (nullable? l)]
    [else
     ; =>
     (let* ([c      (stream-car s)]
            [rest   (stream-cdr s)]
            [dl/dc  (derive c l)]
            [l*     (compact dl/dc)])
       (when debug?
         (display (format "size: ~s; mem: ~s~n" (language-size l*) (current-memory-use))))
       (recognizes? l* rest
              #:compact compact
              #:debug   debug?))]))


; Partially parse a stream; return sub-parses:
(define (parse-partial l s) 
  (if (stream-null? s)
      (parse-null/input l stream-null)
      (match l 
        [(empty)      stream-null]
        [(eps)        (stream (cons (eps) s))]
        [(eps* S)     (parse-null/input l s)]
        [(token p c)
         ; =>
         (cond
           [(p (stream-car s)) (stream-cons (cons (stream-car s) (stream-cdr s))
                                            stream-null)]
           [else               stream-null])]
        [else
         ; =>
         (define c (stream-car s))
         (define rest (stream-cdr s))
         (stream-stitch (parse-partial (parse-derive c l) rest)
                        (parse-null/input l s))])))
                      
; Stream stitching:
(define (stream-stitch s1 s2 #:even [even? #t])
  (define (pull-s1) (stream-cons (stream-car s1) (stream-stitch (stream-cdr s1) s2 #:even (not even?))))
  (define (pull-s2) (stream-cons (stream-car s2) (stream-stitch s1 (stream-cdr s2) #:even (not even?))))
  (cond
    [(and even? (not (stream-null? s1))) (pull-s1)]
    [(and even? (not (stream-null? s2))) (pull-s2)]
    [even?                               stream-null]
    [(not (stream-null? s2))             (pull-s2)]
    [(not (stream-null? s1))             (pull-s1)]
    [else                                stream-null]))

; Checks whether a language is the empty string:
(define/fix (is-null? l)
  #:bottom #t
  (match l
    [(empty)           #f]
    [(eps)             #t]    
    [(token _ _)       #f]
    [(orp l1 l2)       (and (is-null? l1)  (is-null? l2))]
    [(seqp l1 l2)      (and (is-null? l1)  (is-null? l2))]
    [(repp l1)         (or (is-null? l1) (is-empty? l1))]
    [(redp l1 _)       (is-null? l1)]))

(define (set-size s)
  (define size 0)
  (for ([_ s])
    (set! size (+ size 1)))
  size)

(define (singleton? s)
  (eqv? (set-size s) 1))

(define (set-choose s)
  (define el #f)
  (for ([el* s])
    (set! el el*))
  el)

(define-match-expander nullp
  (syntax-rules ()
    [(_)    (app is-null? #t)]
    [(_ el) (and (app is-null? #t) (app parse-null (and (? singleton?) (app set-choose el))))]))

; Checks whether a language is the empty set:
(define/fix (is-empty? l)
  #:bottom #t
  (match l
    [(empty)           #t]
    [(eps)             #f]    
    [(token _ _)       #f]
    [(repp l1)         #f]
    [(orp l1 l2)       (and (is-empty? l1)  (is-empty? l2))]
    [(seqp l1 l2)      (or  (is-empty? l1)  (is-empty? l2))]
    [(redp l1 _)       (is-empty? l1)]))

(define-match-expander emptyp
  (syntax-rules ()
    [(_) (app is-empty? #t)]))




;;;; Optimizations for the grammar:

; Performs top-level reductions on a grammar:
(define/memoize (simplify [l #:eq])
  (match l
    [(empty)       l]
    [(eps)         l]
    [(token p c)   l]
    [(emptyp)      (empty)]
    [(nullp)       (eps* (parse-null l))]

    [(orp (emptyp) l2)  l2]
    [(orp l1 (emptyp))  l1]
    
    [(seqp (nullp t) l2)  (red l2 (lambda (w2) (cons t w2)))]
    [(seqp l1 (nullp t))  (red l1 (lambda (w1) (cons w1 t)))]
    
    [(repp (emptyp))      (eps* (set '()))]
    
    [(redp (and e (nullp)) f) 
     ; =>
     (eps* (for/set ([t (parse-null e)]) (f t)))]
    
    [(redp (seqp (nullp t) l2) f)
     ; =>
     (red l2 (lambda (w2) (f (cons t w2))))]
    
    [(redp (redp l f) g) 
     ; =>
     (red l (compose g f))]
        
    [else    l]))


(define/fix (compactable? l)
  #:bottom #f
  (match l
    [(empty)       #f]
    [(eps)         #f]
    [(emptyp)      #t]
    [(nullp)       #t]
    [(token p c)   #f]

    [(repp (emptyp))  #t]
    [(repp l)         (compactable? l)]
    
    [(orp l1 l2)   (or (compactable? l1) (compactable? l2))]
    [(seqp l1 l2)  (or (compactable? l1) (compactable? l2))]
    
    [(redp (seqp (nullp t) l2) f)   #t]
    [(redp (redp l f) g)            #t]
        
    [(redp l f)    (compactable? l)]))

  
; Performs recursive reductions on a grammar:
; IF YOU UPDATE compact, YOU MUST ALSO UPDATE compactable?
(define/memoize (compact [l #:eq])
  (cond 
    [(not (compactable? l))      l]
    [else
     ; =>
     (match l
       [(empty)       l]
       [(eps)         l]
       [(emptyp)      (empty)]
       [(nullp)       (eps* (parse-null l))]
       [(token p c)   l]
       
       [(repp (emptyp))  (eps* (set '()))]
       [(repp l)         (rep (compact l))]
       
       [(orp (emptyp) l2)  (compact l2)]
       [(orp l1 (emptyp))  (compact l1)]
       
       [(seqp (nullp t) l2)  (red (compact l2) (lambda (w2) (cons t w2)))]
       [(seqp l1 (nullp t))  (red (compact l1) (lambda (w1) (cons w1 t)))]
       
       [(orp l1 l2)   (alt (compact l1) (compact l2))]
       [(seqp l1 l2)  (cat (compact l1) (compact l2))]
       
       [(redp (and e (nullp)) f) 
        ; =>
        (eps* (for/set ([t (parse-null e)]) (f t)))]
       
       [(redp (seqp (nullp t) l2) f)
        ; =>
        (red (compact l2) (lambda (w2) (f (cons t w2))))]
       
       [(redp (redp l f) g) 
        ; =>
        (red (compact l) (compose g f))]
       
       [(redp l f)    (red (compact l) f)])]))



(define/fix (compactable*? l)
  #:bottom #f
  (match l
    [(empty)       #f]
    [(eps)         #f]
    [(token p c)   #f]
    [(emptyp)      #t]
    [(nullp)       #t]

    [(orp l1 l2)   (or (compactable*? l1) (compactable*? l2))]
    [(seqp l1 l2)  (or (compactable*? l1) (compactable*? l2))]
    [(redp l f)    #t]))


; Performs algebraic reductions on a grammar.
; compact* is aggressive and will mangle
; parse trees.  It is only safe for
; recognition.
(define/memoize (compact* [l #:eq])
  (cond
    [(not (compactable*? l))      l]
    [else
     ; => 
     (match l
       [(empty)       l]
       [(eps)         l]
       [(token p c)   l]
       [(emptyp)      (empty)]
       [(nullp)       (eps)]
       
       [(orp (emptyp) l2)  (compact* l2)]
       [(orp l1 (emptyp))  (compact* l1)]
       
       [(seqp (nullp t) l2)  (compact* l2)]
       [(seqp l1 (nullp t))  (compact* l1)]
       
       [(orp l1 l2)   (alt (compact* l1) (compact* l2))]
       [(seqp l1 l2)  (cat (compact* l1) (compact* l2))]
       
       [(redp l f) (compact* l)])]))


;;;; Debugging.

; Gives every object a unique value:
(define mark-of-beast 
  (let* ([index (make-hasheq)]
         [max   0]
         [next  (lambda ()
                  (set! max (+ max 1))
                  max)])
    (lambda (object)
      (if (hash-has-key? index object)
          (hash-ref index object)
          (begin
            (hash-set! index object (next))
            (mark-of-beast object))))))

; Allows recursive functions on graphs by
; turning them into graph searches:
(define-syntax define/search 
  (syntax-rules ()
    [(_ (f x rest ...) #:reentry default body ...)
     ; =>
     (define f (let ([visited (make-parameter #f)]
                     [$def    default])
                 (lambda (x rest ...)
                   (cond
                     [(not (visited))
                      ; =>
                      (parameterize ([visited (make-hasheq)])
                        (f x rest ...))]
                     
                     [(hash-has-key? (visited) x)
                      ; =>
                      (if (procedure? $def) ($def x) $def)]
                     
                     [else 
                      ; =>
                      (hash-set! (visited) x #t)
                      (let () body ...)]))))]
    
    [(_ (f x rest ...) body ...)
     ; =>
     (define/search (f x rest ...) #:reentry (lambda (x) (void)) body ...)]))

; Computes the size of a grammar.
(define/search (language-size l) 
  #:reentry 0
  (match l
    [(or (eps) (token _ _) (empty))   1]
    [(or (seqp l1 l2) (orp l1 l2))    (+ 1 (language-size l1)
                                           (language-size l2))]
    [(or (redp l _) (repp l))         (+ 1 (language-size l))]))
     
; Outputs a grammar as a dot file.
(define (dotify l #:port [port (current-output-port)])
  
  (define/search (dotify-nodes l port)
    (define m (mark-of-beast l))
    (match l
      [(empty) 
       ; =>
       (display (format "\"~s\" [label = \"empty\"~n];~n~n" m) port)]
      
      [(eps* S)
       ; =>
       (display (format "\"~s\" [shape = \"record\", label = \"eps* | ~v\"~n];~n~n" m S) port)]
      
      [(eps)
       ; =>
       (display (format "\"~s\" [label = \"eps\"~n];~n~n" m) port)]
      
      [(token _ c)
       ; =>
       (display (format "\"~s\" [shape = \"record\", label = \"token | ~s\"~n];~n~n" m c) port)]
      
      [(orp l1 l2)
       ; =>
       (define m1 (mark-of-beast l1))
       (define m2 (mark-of-beast l2))
       (display (format "\"~s\" [label = \"or\"~n];~n~n" m) port)
       (dotify-nodes l1 port)
       (dotify-nodes l2 port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m m1) port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m m2) port)]
      
      [(seqp l r)
       ; =>
       (define ml (mark-of-beast l))
       (define mr (mark-of-beast r))
       (display (format "\"~s\" [shape=\"none\", margin=0, label = <~n<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>~n];~n~n" m) port)
       (dotify-nodes l port)
       (dotify-nodes r port)
       (display (format "\"~s\":L -> \"~s\" [~n];~n~n" m ml) port)
       (display (format "\"~s\":R -> \"~s\" [~n];~n~n" m mr) port)]
      
      [(redp l f)
       ; =>
       (define ml (mark-of-beast l))
       (display (format "\"~s\" [label = \"red\"~n];~n~n" m) port)
       (dotify-nodes l port)
       (display (format "\"~s\" -> \"~s\" [~n];~n~n" m ml) port)]))
  
  (define close-port? #f)
  
  (when (string? port)
    (set! close-port? #t)
    (set! port (open-output-file port #:mode 'text #:exists 'replace)))
  
  (display (format "digraph {~n~n") port)
  (display (format "node [];~n") port)
  (dotify-nodes l port)
  (display (format "\"~s\" [shape = \"doublecircle\"~n];~n" (mark-of-beast l)) port)
  (display (format "~n}") port)
  
  (when close-port?
    (close-output-port port)))
           

  

; Converts a grammar into a tree:
(define (language-to-tree l [seen (make-hasheq)])
  
  (define (seen?) (hash-has-key? seen l))
  
  (define (mark) (hash-set! seen l #t))
  
  (if (seen?)
      '-
      (begin 
        (mark)
        (match l
          [(empty)          '(empty)]
          [(eps* S)         `(eps* ,@(for/list ((s S)) s))]
          [(eps)            '(eps)]
          [(token _ class)   (list 'token class)]
          [(seqp l1 l2)     `(seq ,(language-to-tree l1 seen)
                                  ,(language-to-tree l2 seen))]
          [(orp l1 l2)      `(or  ,(language-to-tree l1 seen)
                                  ,(language-to-tree l2 seen))]
          [(redp l1 f)      `(red ,(language-to-tree l1 seen))]))))



;;;; Testing.




(define simple
  (lang (seq 'a 'b)))

(check-expect (token? (force (concatenation-left simple))) #t)

(check-expect (match simple
                [(seqp (token _ x) (token _ y))
                 ; =>
                 (list 'help x y)])
              '(help a b))

(check-expect (nullable? simple) #f)

(define xylist 
  (lang (or (seq 'x yxlist)
            (or 'x (eps)))))

(check-expect (nullable? xylist) #t)

(define yxlist
  (lang (or (seq 'y xylist)
            'y)))

(check-expect (nullable? yxlist) #f)

(define alist 
  (lang (or (seq alist 'a)
            'a)))

(check-expect (nullable? alist) #f)

(define alist??
  (lang (or (seq alist 'a)
            (eps))))

(check-expect (nullable? alist??) #t)

(define elist
  (lang (or (seq elist (eps))
            (eps))))

(define slist
  (lang (or (seq 's slist)
            (eps))))

(define rlist
  (lang (or (--> (seq 'r rlist) (match-lambda [(cons 'r rest) (cons 'a rest)]))
            (eps* (set '())))))

(define llist
  (lang (or (--> (seq llist (? symbol? 'sym)) (match-lambda [(cons front last) (append front (list last))]))
            (eps* (set '())))))

(define nlist
  (lang (or (--> (seq (? integer? 'int) nlist) (lambda (lst) lst))
            (eps* (set '())))))

(define econs
  (lang (seq (eps* (set 'a 'b))
             (eps* (set 'c 'd))
             rlist)))

; (check-expect (parse-null econs) (set '(a c) '(a d) '(b c) '(b d)))

(check-expect (recognizes? xylist (stream 'x 'y 'x 'y)) #t)

(check-expect (recognizes? alist (stream 'a 'a 'a 'b)) #f)

(check-expect (recognizes? alist (stream 'a 'a 'a 'a)) #t)

(check-expect (stream->list (parse-null/input rlist '(e)))
              '((() e)))

(check-expect (recognizes? rlist (stream 'r 'r)) #t)

(check-expect (parse-null (parse-derive 'r rlist)) (set '(a)))

(check-expect (eq? (parse-derive 'r rlist) (parse-derive 'r rlist)) #t)

(check-expect (eq? (parse-derive 'r (parse-derive 'r rlist))
                   (parse-derive 'r (parse-derive 'r rlist)))
              #t)

(check-expect (parse-null (parse-derive 'r (parse-derive 'r rlist)))
              (set '(a a)))

(check-expect (parse-null (parse-derive 'r (parse-derive 'r (parse-derive 'r rlist))))
              (set '(a a a)))

(check-expect (parse rlist (stream 'r 'r 'r 'r 'r 'r 'r))
              (set '(a a a a a a a)))


(check-expect (parse nlist (stream 1 2 3 4 5))
              (set '(1 2 3 4 5)))

(check-expect (parse llist (stream 'a 'b 'c 'd 'e))
              (set '(a b c d e)))



(define t (make-weak-hash-trie #:equal #:eqv))
(check-expect (weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 10] 300) 300)
(check-expect (weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 20] 320) 320)
(check-expect (weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 10] 400) 300)



(test)


;;;; Benchmarks.

(define/memoize (fib [n #:eqv])
  (match n
    [0  1]
    [1  1]
    [n  (+ (fib (- n 1)) (fib (- n 2)))]))

(time (fib 30)) ; testing for time







(define S (lang (or (seq S '+ S)
                    '0
                    '1
                    'N)))

(define good-input '(1 + 1 + 0 + 1 + 1 + 1 + 0 + 0 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + 1 + 0 + 0 + 1 + 1 + 1 + 1 + 0 + 0 + 0 + 1 + 1 + 1 + 1 + 0 + 0 + 1 + 1 + 1 + 0 + 1 + 1 + 1 + 0 + 1 + 0 + 1 + 0))

(define bad-input '(N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + + N))

(display (format "good: ~s~n" (length good-input)))

(display (format "bad: ~s~n" (length bad-input)))

;(time (recognizes? S (list->stream good-input)))

(time (recognizes? S (list->stream bad-input)))



(define SXList (lang (or (seq SX SXList)
                         (eps* (set '())))))

(define SX (lang (or (--> (seq 'lp SXList 'rp) (match-lambda [(cons _ (cons sxlist _)) sxlist]))
                     'atom)))

(define SX* (lang (or (--> (seq 'lp (rep SX*) 'rp)
                           (match-lambda [`(lp ,sxl . rp) sxl]))
                      'atom)))

(define (make-sx-bench n)
  (cond
    [(= n 0) '()]
    [else    (cons 'atom (make-sx-bench (- n 1)))]))

(define sxin `(lp 
               lp lp lp lp lp lp lp lp lp lp atom rp rp rp rp rp rp rp rp rp rp
               ,@(make-sx-bench 50)
               lp atom rp
               ,@(make-sx-bench 50)
               lp lp lp atom rp rp rp
               rp))

(display (format "length: ~s~n" (length sxin)))

(time (void (parse SXList 
                   (list->stream sxin)
                   #:compact compact
                   #:debug   #t)))

(dotify (compact (parse SXList (list->stream sxin) #:steps 10 #:compact compact)) #:port "grammar.dot")


(define E (lang (or (--> (seq T '+ E)
                         (match-lambda [`(,t + . ,e) `(+ ,t ,e)]))
                    T)))

(define T (lang (or (--> (seq F '* T) 
                         (match-lambda [`(,f * . ,t) `(* ,f ,t)]))
                    F)))

(define F (lang (or '1 '0)))

(time (parse E (list->stream '(0 + 1 + 1 * 1 + 0 + 1 * 0 + 0 * 1 * 1 * 0 + 1 + 1 * 1))
             #:debug #f))

(time (parse E (list->stream good-input)
             #:debug #f
             #:compact compact))

(define elist-r (lang (or (seq (eps)  elist)
                        (eps))))

(recognizes? elist (stream (list->stream '(foo))))

(define Inf (lang (red Inf)))

(recognizes? Inf (stream))

(define sx-evil `(lp ,@(make-sx-bench 1000) rp))

(define sx-norm `(lp lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp 
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp 
                     lp ,@(make-sx-bench 100) rp
                     lp ,@(make-sx-bench 100) rp
                     rp))

;(time (parse SX* (list->stream sx-evil)
;             #:compact compact))

;(time (parse SX* (list->stream sx-norm)
;             #:compact compact))


;(time (parse SX  (list->stream sx-evil)
;             #:compact compact))

(define OneList (lang (or (seq OneList '1)
                          (eps* (set '())))))

(define one-in '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

(dotify OneList #:port "one-in-00.dot")

(dotify (parse OneList (list->stream one-in) #:steps 1) #:port "one-in-01.dot")

(dotify (parse OneList (list->stream one-in) #:steps 5) #:port "one-in-05.dot")

(dotify (parse OneList (list->stream one-in) #:steps 10) #:port "one-in-10.dot")

(dotify (parse OneList (list->stream one-in) #:steps 1 #:compact compact) #:port "comp-one-in-01.dot")

(dotify (parse OneList (list->stream one-in) #:steps 5 #:compact compact) #:port "comp-one-in-05.dot")

(dotify (parse OneList (list->stream one-in) #:steps 10 #:compact compact) #:port "comp-one-in-10.dot")
