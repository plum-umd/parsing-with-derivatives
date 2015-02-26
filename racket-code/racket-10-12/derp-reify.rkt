(module derp-reify
  racket
  
  (require 2htdp/image)
  (require "derp-core.rkt")
  (require "derp-zip.rkt")
  
  (provide (all-defined-out))
  
  (define (reify-size L)
    (define reified (make-hasheq))
    (define (seen L) (hash-set! reified L true))
    
    (define size 0)
    (define (bump) (set! size (+ 1 size)))
    
    (let loop ([L L])
      (cond
        [(hash-ref reified L false) (void)]
        [else
         (bump)
         (seen L)
         (match L
           [(∅)            (void)]
           [(ε _)          (void)]
           [(token _)      (void)]
           [(δ L1)         (loop L1)]
           [(∪ L1 L2)      (loop L1) (loop L2)]
           [(∘ L1 L2)      (loop L1) (loop L2)]
           [(★ L1)        (loop L1)]
           [(→ L1 _)       (loop L1)])]))
    
    size)
  
  (define (reify-size/zipper Z)
    (reify-size (zipper-parser Z)))
                          
  
  ; given...
  ; [reified]: a hash-table of reified keys
  ; [get-next-id]: a function to get the next fresh id
  ; [emit]: a function to call with results
  ; returns the reified key of [L] with [emit] having been called on all
  ; recursively processed parseres.
  (define (reify-loop reified get-next-id emit L)
    (let loop ([L L])
      (let ((k (hash-ref reified L false)))
        (if k
            k
            (let ((k (get-next-id)))
              (hash-set! reified L k)
              (match L
                [(∅)         (emit `(,k ∅))]
                [(ε np)      (emit `(,k (ε ,np)))]
                [(token t)   (emit `(,k (token ,t)))]
                [(δ L1)      (emit `(,k (δ ,(loop L1))))]
                [(∪ L1 L2)   (emit `(,k (∪ ,(loop L1) ,(loop L2))))]
                [(∘ L1 L2)    (emit `(,k (∘ ,(loop L1) ,(loop L2))))]
                [(★ L1)      (emit  `(,k (★ ,(loop L1))))]
                [(→ L1 _)    (emit  `(,k (→ ,(loop L1))))])
              k)))))
    
  ; reify a parser 
  (define (reify L)
    (define next-id 0)
    (define (get-next-id)
      (define x next-id)
      (set! next-id (+ 1 next-id))
      x)

    (define results '())
    (define (emit l) (set! results (cons l results)))

    (define reified (make-hasheq))
    
    (define k (reify-loop reified get-next-id emit L))
    
    (list k results))
  
  ; reify a zipper
  (define (reify-zipper Z)
    (define next-id 0)
    (define (get-next-id)
      (define x next-id)
      (set! next-id (+ 1 next-id))
      x)
    
    (define results '())
    (define (emit l) (set! results (cons l results)))
    
    (define reified (make-hasheq))
    
    (define k (reify-loop reified get-next-id emit (zipper-parser Z)))
    (reify-loop reified get-next-id emit (pzip Z))
    
    (list k results))
  
  ; output a graphviz dot file for a reified parser (or zipper)
  (define (dotify Rs)
    (printf "digraph G {~n~n")
    (printf "mode=hier;~n")
    (printf "splines=true;~n")
    (printf "overlap=prism;~n")
    (printf "~n")
    (for-each (lambda (R) (dotify-one (first Rs) R)) (second Rs))
    (printf "}"))
    
  ; dotify a single term
  (define (dotify-one k R)
    (define k-circle (if (equal? (first R) k)
                         "style=bold, color=red, "
                         ""))
    (match R
      [`(,k ∅)               (printf "~s [~alabel = \"∅\"];~n" k k-circle)]
      [`(,k (ε ,np))          (printf "~s [~ashape=record label = \"ε | ~v\"];~n" k k-circle np)]
      [`(,k (token ,t))       (printf "~s [~ashape=record label = \"token | ~v\" ];~n" k k-circle t)]
      [`(,k (δ ,k1))         (begin
                               (printf "~s [~alabel = \"δ\"];~n" k k-circle)
                               (printf "~s -> ~s;~n" k k1))]
      [`(,k (∪ ,k1 ,k2))     (begin
                               (printf "~s [~alabel = \"∪\"];~n" k k-circle)
                               (printf "~s -> ~s;~n" k k1)
                               (printf "~s -> ~s;~n" k k2))]
      [`(,k (∘ ,k1 ,k2))     (begin
                               (printf "~s [~ashape=record label = \"{ ∘ | { <L> L | <R> R } }\"];~n" k k-circle)
                               (printf "~s:L -> ~s;~n" k k1)
                               (printf "~s:R -> ~s;~n" k k2))]
      [`(,k (★ ,k1))         (begin
                               (printf "~s [~alabel = \"★\"];~n" k k-circle)
                               (printf "~s -> ~s;~n" k k1))]
      [`(,k (→ ,k1))         (begin
                               (printf "~s [~alabel = \"→\"];~n" k k-circle)
                               (printf "~s -> ~s;~n" k k1))]))
  
  (define (viz/file Rs path)
    (let ((o (open-output-string)))
      (parameterize ((current-output-port o))
        (dotify Rs))
      (let ((s (get-output-string o)))
        (with-input-from-string s (lambda ()
                                    (system (format "dot -Tpng -o ~s" (path->string path))))))))
    
  ; returns the visualized image given the reification of a parser (or zipper)
  (define (viz Rs)
    (let ((tf (make-temporary-file)))
      (viz/file Rs tf)
      (let ((i (bitmap/file (path->string tf))))
        (delete-file tf)
        i)))
  
  (define (reify/viz/file L path)
    (viz/file (reify L) path))
  
  ; reify and visualize a parser
  (define (reify/viz L)
    (viz (reify L)))

  (define (reify-zipper/viz/file Z path)
    (viz/file (reify-zipper Z) path))
  
  ; reify and visualize a zipper
  (define (reify-zipper/viz Z)
    (viz (reify-zipper Z)))
  
  ; get the size of a reified parser
  #;(define (psize Rs)
    (length (first (rest Rs))))
  
  #;(define (reify/psize L)
    (psize (reify L)))
  
  #;(define (reify-zipper/psize Z)
    (match Z
      [(zipper L C) (reify/psize L)])))
