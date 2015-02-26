#lang racket

(require srfi/41) ; Stream library

(define-struct language ())

; Atomic languages:
(define-struct (empty language) ())           ; empty set
(define-struct (eps   language) ())           ; empty string
(define-struct (token language) (pred class)) ; terminal

(define-struct (compound-language language)
  ([nullable? #:mutable] [d/dc #:mutable]))

; Compound languages:
(define-struct (union language) 
  (this that  [nullable? #:mutable #:auto] [d/dc #:mutable #:auto])
  #:auto-value '())

(define-struct (concatenation language)
  (left right [nullable? #:mutable #:auto] [d/dc #:mutable #:auto])
  #:auto-value '())

(define-struct (reduction language) 
  (name lang  [nullable? #:mutable #:auto] [d/dc #:mutable #:auto] [reduce #:mutable #:auto])
  #:auto-value '())

; Special tokens that can only appear during parsing:
(define-struct (bra language) (name [reduce #:mutable #:auto]))
(define-struct (ket language) (name [reduce #:mutable #:auto]))


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


(define-syntax lang
  (syntax-rules (or quote seq empty reduction eps ?)
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
     (reduction _ (app force (langp l)) _ _ _)]
    
    [(_ (or))
     ; =>
     (empty)]
    
    [(_ (or l1))
     ; =>
     (langp l1)]
    
    [(_ (or l1 l2 ...))
     ; =>
     (union (app force (langp l1))
            (app force (langp (or l2 ...)))
            _ _)]
    
    [(_ (seq))
     ; =>
     (eps)]
    
    [(_ (seq l1))
     ; =>
     (langp l1)]
    
    [(_ (seq l1 l2 ...))
     ; =>
     (concatenation (app force (langp l1))
                    (app force (langp (seq l2 ...)))
                    _ _)]

    [(_ atom)
     ; =>
     atom]))
    

(define-match-expander orp
  (syntax-rules ()
    [(_ l1 l2) (union (app force l1) (app force l2) _ _)]))

(define-match-expander seqp
  (syntax-rules ()
    [(_ l1 l2) (concatenation (app force l1) (app force l2) _ _)]))

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

    [(langp (or l1 l2))
     ; =>
     (let ((current (union-nullable? l)))
       (cond
         [(null? current) (if compute?
                              (compute-nullable l)
                              #f)] ; by default
         [else            current]))]
    
    [(langp (seq l1 l2))
     ; =>
     (let ((current (concatenation-nullable? l)))
       (cond
         [(null? current) (if compute?
                              (compute-nullable l)
                              #f)] ; by default
         [else            current]))]))



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

    [(langp (or l1 l2))
     ; =>
     (let ((current (union-nullable? l)))
       (cond
         [(null? current) 
          ; =>
          (set-union-nullable?! l boolean)
          (mark-change)]
         
         [(not (eq? current boolean))
          ; =>
          (set-union-nullable?! l boolean)
          (mark-change)]
         
         [else (void)]))]
    
    [(langp (seq l1 l2))
     ; =>
     (let ((current (concatenation-nullable? l)))
       (cond
         [(null? current) 
          ; =>
          (set-concatenation-nullable?! l boolean)
          (mark-change)]
         
         [(not (eq? current boolean))
          ; =>
          (set-concatenation-nullable?! l boolean)
          (mark-change)]
         
         [else (void)]))]))
          

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


;(define (parse l s) 
  
;  (match l 
;    [(empty)   stream-null]
;    [(eps)     (stream (cons (eps) s))]


; parse-null

; parse-full

                                  

(define (derive c l)
  
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
    
    ))


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

(nullable? yxlist)

(recognize? xylist (stream 'x 'y 'x 'y))

(language-to-tree xylist)

;(recognize? alist (stream 'a 'a 'a))


