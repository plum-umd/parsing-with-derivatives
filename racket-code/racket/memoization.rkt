(module memoization
  racket/base

  (provide define/memoize)
  
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
  
  
  ; Define a function that is memoized by default:
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
       (define/memoize (f [v #:equal] ...) body ...)])))
  