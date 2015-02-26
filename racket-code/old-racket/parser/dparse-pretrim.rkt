#lang racket

(require srfi/41) ; Stream library

(require test-engine/racket-tests)

(define-struct language ())

; Atomic languages:
(define-struct (empty language) ())           ; empty set
(define-struct (eps   language) ())           ; empty string
(define-struct (token language) (pred class)) ; terminal


; Compound languages:
(define-struct (compound-language language)
  ())

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
     (concatenation (delay l1) (delay (cat l2 ...)))]))


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
     (union (delay l1) (delay (alt l2 ...)))]))

(define-syntax red
  (syntax-rules ()
    [(_ l f) 
     ; =>
     (reduction (delay l) f)]))



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
    

(define-match-expander orp
  (syntax-rules ()
    [(_ l1 l2) (union (app force l1) (app force l2))]))

(define-match-expander seqp
  (syntax-rules ()
    [(_ l1 l2) (concatenation (app force l1) (app force l2))]))

(define-match-expander redp
  (syntax-rules ()
    [(_ l f) (reduction (app force l) f)]))

(define-match-expander tokenp
  (syntax-rules ()
    [(_ pred class) (token pred class)]
    [(_ class) (token _ class)]))

(define-match-expander nullablep?
  (syntax-rules ()
    [(_) (app nullable? #t)]))



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
                     (visited   (make-parameter 'error-visited))
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
                          ;(display (format "updated ~v: ~v/~v~n" x new-val cached))
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
                          

(define/fix (nullable? l)
  #:bottom #f
  (match l
    [(empty)     #f]
    [(eps)       #t]    
    [(token _ _) #f]
    [(orp l1 l2) 
     ; =>
     (or (nullable? l1) (nullable? l2))]
    [(seqp l1 l2)
     ; =>
     (and (nullable? l1) (nullable? l2))]
    [(redp l1 _)
     ; =>
     (nullable? l1)]))


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
       
  


(define/memoize (parse-derive c l)
  #:order ([l #:eq] [c #:equal])
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


(define/memoize (derive c l)
  #:order ([l #:eq] [c #:equal])
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
     (cat (derive c l1) l2)]
    
    [(redp l f)
     ; =>
     (derive c l)]))


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


(define (recognize? l s)
  (cond
    [(stream-null? s) (nullable? l)]
    [else             (recognize?
                       (derive (stream-car s) l)
                       (stream-cdr s))]))






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

(check-expect (recognize? xylist (stream 'x 'y 'x 'y)) #t)

(check-expect (recognize? alist (stream 'a 'a 'a 'b)) #f)

(check-expect (recognize? alist (stream 'a 'a 'a 'a)) #t)

(check-expect (stream->list (parse-null/input rlist '(e)))
              '((() e)))

(check-expect (stream->list (stream-stitch (stream 1 2 3) (stream 4 5 6)))
              '(1 4 2 5 3 6))

(check-expect (recognize? rlist (stream 'r 'r)) #t)

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


;(define t (make-weak-hash-trie #:equal #:eqv))

;(weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 10] 300)
;(weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 20] 320)
;(weak-hash-trie-get! t [#:equal #:eqv] ['(1 2 3) 10] 400)

(define/memoize (fib [n #:eqv])
  (match n
    [0  1]
    [1  1]
    [n  (+ (fib (- n 1)) (fib (- n 2)))]))

(check-expect (fib 20) (fib 20))


(test)