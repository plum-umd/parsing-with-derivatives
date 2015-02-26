#lang racket

(require srfi/41) ; Stream library

(define-struct language ())

; Atomic languages:
(define-struct (empty language) ())           ; empty set
(define-struct (eps   language) ())           ; empty string
(define-struct (token language) (pred class)) ; terminal


; Compound languages:
(define-struct (compound-language language)
  ([nullable? #:mutable] [d/dc #:mutable]))

(define-struct (union compound-language) 
  (this that))

(define-struct (concatenation compound-language)
  (left right))

(define-struct (reduction compound-language) 
  (lang reduce))

; Special tokens that can only appear during parsing:
(define-struct (eps*  eps)  (tree)) ; empty string that produces a tree

;(define-struct (bra language) (name [reduce #:mutable #:auto]))
;(define-struct (ket language) (name [reduce #:mutable #:auto]))


(define-syntax cat
  (syntax-rules ()
    [(_) 
     ; =>
     (eps)]
    
    [(_ l1) 
     ; => 
     l1]
    
    [(_ l1 l2 ...) 
     ; =>
     (concatenation '() (make-weak-hash) (delay l1) (delay (cat l2 ...)))]))


(define-syntax alt
  (syntax-rules ()
    [(_) 
     ; =>
     (empty)]
    
    [(_ l1) 
     ; => 
     l1]
    
    [(_ l1 l2 ...) 
     ; =>
     (union '() (make-weak-hash) (delay l1) (delay (alt l2 ...)))]))

(define-syntax red
  (syntax-rules ()
    [(_ l f) 
     ; =>
     (reduction '() (make-weak-hash) (delay l) f)]))



(define-syntax lang
  (syntax-rules (or quote seq --> empty reduction eps ?)
    [(_)
     ; =>
     (empty)]
    
    [(_ (or))
     ; =>
     (empty)]
    
    [(_ (or l1))
     ; =>
     (lang l1)]
        
    [(_ (or l1 l2 ...))
     ; =>
     (alt (lang l1) (lang (or l2 ...)))]
    
    [(_ (seq))
     ; =>
     (eps)]
    
    [(_ (seq l1))
     ; =>
     (lang l1)]
    
    [(_ (seq first rest ...))
     ; =>
     (cat (lang first) (lang (seq rest ...)))]
    
    [(_ (--> l f))
     ; =>
     (red (lang l) f)]
    
    [(_ (? pred class))
     ; =>
     (token pred class)]
        
    [(_ (eps))
     ; =>
     (eps)]
    
    [(_ (empty))
     ; =>
     (empty)]
    
    [(_ (quote lit))
     ; =>
     (token (lambda (t) (equal? t 'lit)) 'lit)]
    
    [(_ var)
     ; =>
     var]))
    
    

(define-match-expander langp
  (syntax-rules (or seq empty eps token reduction nullable?)
    [(_)
     ; =>
     (empty)]
    
    [(_ (nullable?))
     ; =>
     (? nullable?)]
    
    [(_ (empty))
     ; =>
     (empty)]
    
    [(_ (eps))
     ; =>
     (eps)]
    
    [(_ (token pred class))
     ; =>
     (token pred class)]
    
    [(_ (token))
     ; =>
     (token _ _)]
    
    [(_ (reduction l))
     ; =>
     (reduction _ _ (app force (langp l)) _)]
    
    [(_ (reduction l f))
     ; =>
     (reduction _ _ (app force (langp l)) f)]
    
    [(_ (or))
     ; =>
     (empty)]
    
    [(_ (or l1))
     ; =>
     (langp l1)]
    
    [(_ (or l1 l2 ...))
     ; =>
     (union _ _ (app force (langp l1))
                (app force (langp (or l2 ...))))]
    
    [(_ (seq))
     ; =>
     (eps)]
    
    [(_ (seq l1))
     ; =>
     (langp l1)]
    
    [(_ (seq l1 l2 ...))
     ; =>
     (concatenation _ _ (app force (langp l1))
                        (app force (langp (seq l2 ...))))]

    [(_ atom)
     ; =>
     atom]))
    

(define-match-expander orp
  (syntax-rules ()
    [(_ l1 l2) (union _ _ (app force l1) (app force l2))]))

(define-match-expander seqp
  (syntax-rules ()
    [(_ l1 l2) (concatenation _ _ (app force l1) (app force l2))]))

(define-match-expander redp
  (syntax-rules ()
    [(_ l f) (reduction _ _ (app force l) f)]))

(define-match-expander tokenp
  (syntax-rules ()
    [(_ pred class) (token pred class)]
    [(_ class) (token _ class)]))

(define-match-expander nullablep?
  (syntax-rules ()
    [(_) (? nullable?)]))


(define (nullable? l [compute? #t])
  (match l
    [(empty)
     ; =>
     #f]
    
    [(eps)
     ; =>
     #t]
    
    [(token _ _)
     ; =>
     #f]

    [(compound-language current _)
     ; =>
     (cond
       [(null? current) (if compute?
                            (compute-nullable l)
                            #f)] ; by default
       [else            current])]))
     


(define (set-nullable! l boolean #:mark-changed [change-box #f])

  (define (mark-change)
    (when change-box
      (set-box! change-box #t)))
  
  (match l
    [(empty)
     ; =>
     (error "can't set nullable on empty")]
    
    [(eps)
     ; =>
     (error "can't set nullable on null")]
    
    [(token _ _)
     ; =>
     (error "can't set nullable on token")]

    [(compound-language current _)
     ; =>
     (cond
       [(null? current) 
        ; =>
        (set-compound-language-nullable?! l boolean)
        (mark-change)]
       
       [(not (eq? current boolean))
        ; =>
        (set-compound-language-nullable?! l boolean)
        (mark-change)]
       
       [else (void)])]))
          

(define (for-each-sub-language l f)
  
  (define visited (make-hasheq))
  
  (define (seen? l)
    (hash-has-key? visited l))
  
  (define (mark l)
    (hash-set! visited l #t))
  
  (define (visit l)
    (when (not (seen? l))
      (mark l)
      (match l
        [(or (token _ _) (empty) (eps))
         ; =>
         (void)]
        
        [(langp (reduction l1))
         ; =>
         (visit l1)
         (f l)]
        
        [(langp (or l1 l2)) 
         ; =>
         (visit l1)
         (visit l2)
         (f l)]
        
        [(langp (seq l1 l2))
         ; =>
         (visit l1)
         (visit l2)
         (f l)])))
  
  (visit l))
    

(define (compute-nullable l)
  
  (define change-box (box #f))
  
  (for-each-sub-language l
    (lambda (l)
      (recompute-nullability l change-box)))
  
  (if (unbox change-box)
      (compute-nullable l)
      (nullable? l #f)))
                         
    

(define (recompute-nullability l change-box)
  (match l
    [(or (token _ _) (empty) (eps))
     ; =>
     (void)]
    
    [(langp (reduction l1))
     ; =>
     (set-nullable! l (nullable? l1 #f) #:mark-changed change-box)]
    
    [(langp (or l1 l2))
     ; =>
     (set-nullable! l (or (nullable? l1 #f) (nullable? l2 #f)) #:mark-changed change-box)]
    
    [(langp (seq l1 l2))
     ; =>
     (set-nullable! l (and (nullable? l1 #f) (nullable? l2 #f)) #:mark-changed change-box)]
    
    ))


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
           ;[(stream-null? s)   stream-null]
           [(p (stream-car s)) (stream-cons (cons (stream-car s) (stream-cdr s))
                                            stream-null)]
           [else               stream-null])]
        [else
         ; =>
         (define c (stream-car s))
         (define rest (stream-cdr s))
         (stream-stitch (parse-partial (parse-derive c l) rest)
                        (parse-null/input l s))])))
                           
    
(define (parse l s)
  (cond
    [(stream-null? s) (parse-null l)]
    [else             (parse (parse-derive (stream-car s) l) (stream-cdr s))]))

(define (parse-null/input l input)
  (list->stream (set-map (parse-null l) (lambda (el) (cons el input)))))

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

; parse-null

; parse-full
 
    

(define (cached-derivative c l)
  (match l
    [(compound-language _ d/dc) 
     ; =>
     (hash-ref d/dc c #f)]
    
    [else #f]))
      
(define (set-cached-derivative! c l dl/dc)
  (match l
    [(compound-language _ d/dc)
     ; =>
     (hash-set! d/dc c dl/dc)]
    
    [else (void)]))


;(define (Y/memoize F)
;  (define cache (make-weak-hash))
;  
;  (define f (lambda args 
;              (

(define-syntax while
  (syntax-rules ()
    [(_ cond body ...)
     ; =>
     (letrec ((lp (lambda () (when cond body ... (lp)))))
       (lp))]))



(define define/fix-changed? (make-parameter 'error-changed))
(define define/fix-running? (make-parameter #f))
(define define/fix-visited  (make-parameter 'error-visited))

(define-syntax define/fix
  (syntax-rules ()
    [(_ (f x) #:bottom bottom body ...)
     ; =>
     (define f (let ((cache     (make-weak-hasheq))
                     (changed?  define/fix-changed?)
                     (running?  define/fix-running?)
                     (visited   define/fix-visited)
                     (bot       bottom))
                 (lambda (x)
                   ;(display (format "running on ~s~n" x))
                   (let ((cached? (hash-has-key? cache x))
                         (cached  (hash-ref cache x (lambda () bot)))
                         (run?    (running?)))
                     (cond
                       [(and cached? (not run?))
                        ; =>
                        ;(display (format "cached~s: ~s~n" x cached)) 
                        cached]
                       
                       [(and run? (hash-has-key? (unbox (visited)) x))
                        ; =>
                        ;(display (format "revisiting: ~s~n" x))
                        (if cached? cached bottom)]
                       
                       [run? 
                        ; =>
                        ;(display (format "visiting ~s: ~s~n" x cached))
                        (hash-set! (unbox (visited)) x #t)
                        (let ((new-val (begin body ...)))
                          (when (not (equal? new-val cached))
                            (set-box! (changed?) #t)
                            (hash-set! cache x new-val))
                          ;(display (format "updated ~s: ~s~n" x new-val))
                          new-val)]
                       
                       [(and (not cached?) (not run?))
                        ; =>
                        ;(display (format "initiating~n"))
                        (parameterize ([changed? (box #t)]
                                       [running? #t]
                                       [visited (box (make-weak-hasheq))])
                          (let ([v bot])
                            (while (unbox (changed?))
                                   (set-box! (changed?) #f)
                                   (set-box! (visited) (make-weak-hasheq))
                                   ;(display (format "running again~n"))
                                   (set! v (f x)))
                            v))])))))]))
                          



(define/fix (is-null? l)
  #:bottom #t
  (match l
    [(empty)     #f]
    [(eps)       #t]    
    [(token _ _) #f]
    [(orp l1 l2) 
     ; =>
     (and (is-null? l1) (is-null? l2))]
    [(seqp l1 l2)
     ; =>
     (and (is-null? l1) (is-null? l2))]
    [(redp l1 _)
     ; =>
     (is-null? l1)]))

(define empty-tree-set (set))


(define/fix (parse-null l)
  #:bottom (set)
  (match l
    [(empty)      empty-tree-set]
    [(eps* S)     S]
    [(eps)        (set l)]
    [(token _ _)  empty-tree-set]
    [(orp l1 l2)  
     ; =>
     (set-union (parse-null l1) (parse-null l2))]
    [(seqp l1 l2)
     ; =>
     (for*/set ([t1 (parse-null l1)]
                [t2 (parse-null l2)])
               (cons t1 t2))]
    [(redp l1 f)
     ; =>
     (for/set ([t (parse-null l1)])
              (f t))]))
               
    
    



(define (parse-derive c l)
  (define cached (cached-derivative c l))
  (if
   cached
   cached
   (let ()
     (define dl/dc
       (match l
         [(empty)
          ; =>
          (empty)]
         
         [(eps)
          ; =>
          (empty)]
         
         [(token pred class)
          ; =>
          (if (pred c) (eps* (set c)) (empty))]
         
         [(orp l1 l2)
          ; =>
          (alt (parse-derive c l1) 
               (parse-derive c l2))]
         
         [(seqp (and (nullablep?) l1) l2)
          ; =>
          (alt (cat (eps* (parse-null l1)) (parse-derive c l2))
               (cat (parse-derive c l1) l2))]
         
         [(seqp l1 l2)
          ; =>
          (cat (parse-derive c l1) l2)]
         
         [(redp l f)
          ; =>
          (red (parse-derive c l) f)]))
     (set-cached-derivative! c l dl/dc)
     dl/dc)))
  

; (define/memoize (derive [c #:eqv] [l #:eq]
  
(define-syntax make-weak-hash-trie
  (syntax-rules ()
    [(_ #:eq eq ...)
     ; =>
     (make-weak-hasheq)]
    
    [(_ #:eqv eq ...)
     ; =>
     (make-weak-hasheqv)]
    
    [(_ #:equal eq ...)
     ; =>
     (make-weak-hash)]))

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
     (define f (let ((cache (make-weak-hash-trie eq ...))
                     ($f    (lambda (v ...) body ...)))
                 (lambda (v ...)
                   (weak-hash-trie-get! cache [eq ...] [v ...] ($f v ...)))))]
    
    [(_ (f v ...) body ...)
     ; =>
     (define/memoize (f [v #:equal] ...) body ...)]))
       
  

(define (derive c l)
  (define cached (cached-derivative c l))
  (if
   cached
   cached
   (let ()
     (define dl/dc
       (match l
         [(empty)
          ; =>
          (empty)]
         
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
         
         [(seqp (and (nullablep?) l1) l2)
          ; =>
          (alt (derive c l2)
               (cat (derive c l1) l2))]
         
         [(seqp l1 l2)
          ; =>
          (cat (derive c l1) l2)]))
     (set-cached-derivative! c l dl/dc)
     dl/dc)))


(define (language-to-tree l [seen (make-hasheq)])
  
  (define (seen?) (hash-has-key? seen l))
  
  (define (mark) (hash-set! seen l #t))
  
  (if (seen?)
      '-
      (begin 
        (mark)
        (match l
          [(empty)          '(empty)]
          [(eps)            '(eps)]
          [(token _ class)  (list 'token class)]
          [(seqp l1 l2)     `(seq ,(language-to-tree l1 seen)
                                  ,(language-to-tree l2 seen))]
          [(orp l1 l2)      `(or  ,(language-to-tree l1 seen)
                                  ,(language-to-tree l2 seen))]))))


(define (recognize? l s)
  (cond
    [(stream-null? s) (nullable? l)]
    [else             (recognize?
                       (derive (stream-car s) l)
                       (stream-cdr s))]))






(define simple
  (lang (seq 'a 'b)))

(force (concatenation-left simple))

(match simple
  [(langp (seq x y))
   ; =>
   (list 'help x y)]
  
  [(concatenation x y _ _)
   ; =>
   (list 'manual x y)])



simple

(nullable? simple)

(define xylist 
  (lang (or (seq 'x yxlist)
            (or 'x (eps)))))

(define yxlist
  (lang (or (seq 'y xylist)
            'y)))

(define test 
  (lang (or 'x (eps))))

(define alist 
  (lang (or (seq alist 'a)
            'a)))

(define alist??
  (lang (or (seq alist 'a)
            (eps))))

(define elist
  (lang (or (seq elist (eps))
            (eps))))

(define rlist
  (lang (or (--> (seq 'r rlist) (match-lambda [(cons 'r rest) (cons 'a rest)]))
            (eps* (set '())))))

(define llist
  (lang (or (--> (seq llist 'l) (match-lambda [(cons front last) (append front (list last))]))
            (eps* (set '())))))

(define nlist
  (lang (or (--> (seq (? integer? 'int) nlist) (lambda (lst) lst))
            (eps* (set '())))))


(define econs
  (lang (seq (eps* (set 'a 'b))
             (eps* (set 'c 'd))
             rlist)))

(display "parsing null on econs:")
(newline)
(parse-null econs)        
(newline)

(nullable? yxlist)

(recognize? xylist (stream 'x 'y 'x 'y))

(language-to-tree xylist)

(recognize? alist (stream 'a 'a 'a 'b))


(stream->list (parse-null/input rlist 'e))


(stream->list (stream-stitch (stream 1 2 3) (stream 4 5 6)))

(parse rlist (stream 'r 'r 'r))

(parse nlist (stream 1 2 3 4))

(parse llist (stream 'l 'l 'l))


(define t (make-weak-hash-trie #:equal #:eqv))



(weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 10] 300)
(weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 20] 320)
(weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 10] 400)



(define/memoize (fib [n #:eqv])
  (match n
    [0  1]
    [1  1]
    [n  (+ (fib (- n 1)) (fib (- n 2)))]))

      
                