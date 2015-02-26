(module fixed-points
  racket/base
  
  (provide define/fix)

  ; Generic tools:
  (define-syntax while
    (syntax-rules ()
      [(_ cond body ...)
       ; =>
       (letrec ((lp (λ () (when cond body ... (lp)))))
         (lp))]))
  
  ; Define a recursive (yet monotonic) function over
  ; a mutually recursive graph by computing its fixed
  ; point:
  (define-syntax define/fix
    (syntax-rules ()
      [(_ (f x) #:bottom bottom body ...)
       ; =>
       (define f (let ((cache     (make-weak-hasheq))
                       (changed?  (make-parameter 'error-changed))
                       (running?  (make-parameter #f))
                       (visited   (make-parameter 'error-visited)))
                   (λ (x)
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
                              v))])))))])))
  
