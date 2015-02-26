#lang racket

(require rackunit)

(require "derp-core.rkt")
(require "derp-sugar.rkt")

(check-pred 
 (λ (L)
   (match L
     [(ε S) (equal? S (set 'a))]
     [else  #f]))
 (D 'a (token (λ (t) (eqv? t 'a)))) (ε (set 'a)))

(check-equal?
 (parse '(a) (token (λ (t) (eqv? t 'a))))
 (set 'a))

(check-equal?
 (parse '(a) (lang 'a))
 (set 'a))

(define ab* (lang (∪ (∘ (∪ 'a 'b) ab*)
                     (ε '()))))

(check-equal? 
 (parse '(a b b) ab*)
 (set '(a b b)))

(define ab*-rev (lang (∪ (@--> (list ab*-rev (∪ 'a 'b))
                               (λ (lst hd) (cons hd lst)))
                         (ε '()))))
                         
(check-equal? 
 (parse '(b a a) ab*-rev)
 (set '(a a b)))
              
(define tag (lang `(< ,'tag-name >)))

(check-equal?
  (parse '(< tag-name >) tag)
  (set '(< tag-name >)))


(check-equal?
 (parse '(< id >) (lang (list! '< ,'id '>)))
 (set '(id)))

(let ([literal->language
       (λ (lit) (token (λ (tok) (equal? tok 3))))])
  (check-equal? 
   (parse '(3 3 3 3) (lang (★ 400)))
   (set '(3 3 3 3))))
  
(define (numeric-literals lit)
  (match lit
    ['NUM    (token number?)]
    [else    (literal->language lit)]))
  







(define-grammar left-recursive-grammar
  #:start L
  #:literals numeric-literals
  [L    (∪ (∘ L 'NUM)
           (ε))])

       
;(parse '(1 2 3) left-recursive-grammar)







(define-grammar right-recursive-grammar
  #:start L
  #:literals numeric-literals
  [L  (∪ (∘ 'NUM L)
          (ε))])

;(parse '(1 2 3) right-recursive-grammar)










(define-grammar hidden-left-recursive-grammar
  #:start A
  #:literals numeric-literals
  [A   (∪ (∘ B 'NUM)
          (ε))]
  [B   A])
        
;(parse '(1 2 3) hidden-left-recursive-grammar)
   






        

(define-grammar hidden-right-recursive-grammar
  #:start A
  #:literals numeric-literals
  [A   (∪ (∘ 'NUM B)
          (ε))]
  [B   A])
        
;(parse '(1 2 3) hidden-right-recursive-grammar)








(define-grammar more-hidden-left-recursive-grammar
  #:start A
  #:literals numeric-literals
  [A   (∪ (∘ B 'NUM)
          (ε))]
  [B   (∘ A 'NUM)])
        
;(parse '(1 2 3 4) more-hidden-left-recursive-grammar)
   






(define-grammar infinite-recursive-grammar
   #:start A
  [A   A])

;(parse '(1 2 3) infinite-recursive-grammar) 
;(parse '() infinite-recursive-grammar) 









(define hidden-infinite-recursive-grammar
  (grammar
   #:start A
   [A   B]
   [B   A]))

;(parse '(1 2 3) hidden-infinite-recursive-grammar)







(define trick-middle-infinite-recursive-grammar
  (grammar
   #:start A
   [A    (∘ 'x A 'x)]))

;(parse '(x x x) trick-middle-infinite-recursive-grammar)




(define parseable-infinite-recursive-grammar
  (grammar
   #:start C
   [A    B]
   [B    A]
   [C    (∪ A B (★ 'x))]))

;(parse '(x x x) parseable-infinite-recursive-grammar)







(define amb-exp-grammar
  (grammar
   #:start E
   #:literals numeric-literals 
   [E  (∪ 'NUM
          (@--> (list 'L  E 'R)    (λ (_1 e _2) e))
          (@--> (list E  '+  E)    (λ (e1 _ e2) `(+ ,e1 ,e2)))
          (@--> (list E  '*  E)    (λ (e1 _ e2) `(* ,e1 ,e2))))]))

;(parse '(1 + 2) amb-exp-grammar)
;(parse '(1 + 2 + 3) amb-exp-grammar)
;(parse '(1 + 2 + 3 + 4) amb-exp-grammar)  








(define amb-exp-grammar-with-infinite-recursion
  (grammar
   #:start E
   #:literals numeric-literals 
   [E  (∪ 'NUM
          E
          (@--> (list 'L  E 'R)    (λ (_1 e _2) e))
          (@--> (list E  '+  E)    (λ (e1 _ e2) `(+ ,e1 ,e2)))
          (@--> (list E  '*  E)    (λ (e1 _ e2) `(* ,e1 ,e2))))]))

(parse '(1 + 2 + 3 + 4) amb-exp-grammar-with-infinite-recursion)



