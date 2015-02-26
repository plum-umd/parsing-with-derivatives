#lang racket

(require profile)

(require "derp-core.rkt")
(require "derp-optimize.rkt")
(require "derp-zip.rkt")
(require "derp-reify.rkt")

; Examples

; helper repetition combinator
(define (repeat x) 
  (define result (∪ (∘ x result) 
                    (ε (set '()))))
  result)

;----- regular language x* using either left or right recursion: xxx -----

(define (mk-xs-left)
  (define xs-left (∪ (∘ xs-left (token 'x))
                     (ε (set '()))))
  xs-left)

(define (mk-xs-right)
  (define xs-right (∪ (∘ (token 'x) xs-right)
                      (ε (set '()))))
  xs-right)

; a sequence of xs with length n
(define (gen-xs-input n)
  (if (equal? n 0)
      '()
      `(x . ,(gen-xs-input (- n 1)))))

;----- nested parentheses: ((())) -----

(define (mk-parens) 
  (define parens (∪ (∘ (token 'LP) (∘ parens (token 'RP)))
                    (ε (set '()))))
  parens)

; a sequence of nested parentheses with context-depth n (length 2n)
(define (gen-parens-input n)
  (let loop ([n (truncate (/ n 2))])
    (if (equal? n 0)
        '()
        `(LP ,@(loop (- n 1)) RP))))

;----- s-expressions: (s s (s s)) -----

(define (mk-sexp)
  (define sexp (∪ (∘ (token 'LP) (∘ many-sexp (token 'RP)))
     (token 's)))
  (define many-sexp (repeat sexp))
  sexp)

; an s-expression with context-depth n
(define (gen-sexp-input n)
  (match n
    [0 '(s)]
    [1 '(s)]
    [2 '(LP RP)]
    [3 '(LP s RP)]
    [_
     
     (let loop ([n (truncate (/ n 4))])
       (if (equal? n 0)
           '()
           `(LP
             s
             ,@(loop (- n 1)) 
             s
             RP)))]))
  
;----- ambiguous add expression: 1 + 1 + 1 + 1

(define (mk-add-expr) 
  (define add-expr (∪ (∘ add-expr (∘ (token '+) add-expr))
                      (token 1)))
  add-expr)

; an add expression with n 1s
(define (gen-add-expr-input n)
  (let loop ([n (truncate (/ n 2))])
    (if (equal? n 0)
        '(1)
        `(1 + . ,(loop (- n 1))))))
  
;----- ambiguous context-free add expression: (1 + [1 + (1)] + 1)

(define (mk-cf-add-expr) 
  (define cf-add-expr (∪ (∘ (token 'LP1) (∘ cf-add-expr (token 'RP1)))
                         (∪ (∘ (token 'LP2) (∘ cf-add-expr (token 'RP2)))
                            (∪ (∘ cf-add-expr (∘ (token '+) cf-add-expr))
                               (token 1)))))
  cf-add-expr)
  
; a context-free add expression with context-depth+ambiguity = n
(define (gen-cf-add-expr-input n)
  (match n
    [0 '(1)]
    [1 '(1)]
    [2 '(1)]
    [3 '(1 + 1)]
    [4 '(1 + 1)]
    [5 '(LP1 1 RP1 + 1)]
    [6 '(LP1 1 RP1 + 1)]
    [7 '(LP2 LP1 1 RP1 + 1 RP2)]
    [_
     (let loop ([n (truncate (/ n 8))])
       (if (equal? n 0)
           '(1)
           `(LP2
             1
             +
             LP1 
             ,@(loop (- n 1))
             RP1 
             +
             1
             RP2)))]))

;----- ambiguous to parse a or b after xs: xxxxxxa, xxxb

(define (mk-after-parens)
  (define parens (mk-parens))
  (define a-after-parens (∘ parens (token 'a)))
  (define b-after-parens (∘ parens (token 'b)))
  (∪ a-after-parens b-after-parens))

(define (gen-after-parens-input n)
  `(,@(gen-parens-input n) a))
                       

;----- some basic preliminary demos -----

(define xs-input '(x x x x x x x x x x x))
  

(define parens-input '(LP LP LP LP RP RP RP RP))

(parse xs-input (mk-xs-left))
(parse xs-input (mk-xs-right))
(parse parens-input (mk-parens))

(parse/compact xs-input (mk-xs-left))
(parse/compact xs-input (mk-xs-right))
(parse/compact parens-input (mk-parens))

(parse/zip xs-input (mk-xs-left))
(parse/zip xs-input (mk-xs-right))
(parse/zip parens-input (mk-parens))

(define steps 4)

; beware, some of these visualizations are huge!

#;(reify/viz (D/n steps xs-input xs-left))
#;(reify/viz (D/compact/n steps xs-input xs-left))
#;(reify-zipper/viz (D/zip/n steps xs-input xs-left))

#;(reify/viz (D/n steps xs-input xs-right))
#;(reify/viz (D/compact/n steps xs-input xs-right))
#;(reify-zipper/viz (D/zip/n steps xs-input xs-right))

#;(reify/viz (D/n steps parens-input parens))
#;(reify/viz (D/compact/n steps parens-input parens))
#;(reify-zipper/viz (D/zip/n steps parens-input parens))

;----- micro benchmarks -----

(define short-circuit 1000)
(define test-iters 1)

(define to-test
  (list
   (list 'xs-left gen-xs-input mk-xs-left)
   (list 'xs-right gen-xs-input mk-xs-right)
   (list 'parens gen-parens-input mk-parens)
   (list 'sexp gen-sexp-input mk-sexp)
   (list 'add-expr gen-add-expr-input mk-add-expr)
   (list 'cf-add-expr gen-cf-add-expr-input mk-cf-add-expr)
   (list 'after-parens gen-after-parens-input mk-after-parens)))

(define sizes
  (list 1 2 3 4 5 6 7 8 9 10 20 50 100 200 500 1000)
  #;(list 1 2 3 4 5 6 7 8 9 10 20 50))

(define (concat lls)
  (match lls
    ['() '()]
    [(cons l ls) (append l (concat ls))]))

(define inputs
  (let ((result (concat (map
                         (λ (n)
                           (map
                            (λ (t)
                              (list n (first t) ((second t) n)))
                            to-test))
                         sizes))))
    (make-immutable-hash (map (λ (x) 
                                (cons (cons (first x) (second x)) (third x))) 
                              result))))

(define (bench p p-stats n input-name input-gen mk-grammar #:include-parse-result? [ipr false])
  (define input (hash-ref inputs (cons n input-name)))
  (define stats (p-stats input (mk-grammar)))
  
  (cond
    [(member 'too-large stats)
     (eprintf "finished ~s size ~s~n" input-name n)
     (write 
     `((input-length ,(length input))
       (grammar-size ,stats)))
     (printf "~n")]
    [else
     (define-values (final-timing parse-result)
       (let loop ([i test-iters] [timings (list)] [last-result false])
         (cond
           [(equal? i 0)
            (values timings last-result)]
           [else
            (define g (mk-grammar))
            (collect-garbage)
            (define-values (parse-result cpu-time real-time gc-time) 
              (time-apply (λ () (p input g)) '()))
            (cond
              [(equal? parse-result (list (set)))
               (eprintf "could not parse ~a ~a~n" input-name input)
               (flush-output (current-error-port))
               'did-not-parse]
              [else
                (loop (- i 1) (cons (cons cpu-time gc-time) timings) parse-result)])])))
     (eprintf "finished ~s size ~s~n" input-name n)
     (write 
     `((time ,final-timing)
       (input-length ,(length input))
       (grammar-size ,stats)
       ,@(if ipr `((parse-result ,parse-result)) '())))
     (printf "~n")]))


(define (parse-stats input p)
  (define size (reify-size p))
  (if (> size short-circuit)
      '(too-large)
      (cons size
            (if (null? input)
                '()
                (parse-stats (cdr input) (D (car input) p))))))
  
(define (parse/compact-stats input p)
  (define size (reify-size p))
  (if (> size short-circuit)
      '(too-large)
      (cons size
            (if (null? input)
                '()
                (parse/compact-stats (cdr input) (K (D (car input) p)))))))

(define (parse/zip-stats input p) (parse/zip-stats/zipper input (zipper p 'top)))
(define (parse/zip-stats/zipper input z)
  (define size (reify-size/zipper z))
  (if (> size short-circuit)
      '(too-large)
      (cons size
            (if (null? input)
                '()
                (parse/zip-stats/zipper (cdr input)
                                        (drill
                                         (regress
                                          (match z
                                            [(zipper p k) (zipper (K (D (car input) p)) k)]))))))))

(define algorithms
  (list
   (list 'naive parse parse-stats)
   (list 'compaction parse/compact parse/compact-stats)
   (list 'zipper parse/zip parse/zip-stats)))

(define (run-bench)
  (define results-path (string->path "results.rktd"))
  (if (file-exists? results-path) 
      (delete-file results-path)
      (void))
  (define out (open-output-file results-path))
  (parameterize ([current-output-port out])
    (printf "(~n")
    (write inputs)
    (for-each
     (λ (alg)
       (match alg
         [(list name mk-p p-stats)
          (printf "~n(~a~n" name)
          (for-each
           (λ (args)
             (printf "~n(~a~n" (first args))
             (for-each
              (λ (n)
                (list n
                      (apply bench (append (list mk-p p-stats n) args))))
              sizes)
             (printf ")~n"))
           to-test)
          (printf ")~n")]))
     algorithms)
    (printf ")~n"))
  (close-output-port out))
  
;----- old testing code -----

#;(define (see-after-parens n)
  (reify-zipper/viz (D/zip/n n '(LP LP LP RP RP RP b) after-parens)))
#;(see-after-parens 3)
#;(define (see-after-parens-file n file-name)
  (reify-zipper/viz/file (D/zip/n n '(LP LP LP LP RP RP RP RP b) after-parens) (string->path file-name)))

;----- failing for unknown reasons -----
#;(parse/compact '(LP1 1 RP1 + LP1 1 RP1 + LP1 1 RP1 + LP1 1 RP1 + LP1 1 RP1) (mk-cf-add-expr))

;----- processing results file -----
(define (avg xs)
  (/ (foldl + 0 xs) (length xs)))

(define (mangle-results results)
  (set! results (rest results))
  (define times 
    (concat (map 
     (λ (algo-results)
       (match algo-results
         [`(,algo-name . ,result-list)
          (map
           (λ (result-set)
             (match result-set
               [`(,test-name . ,test-results)
                `((,algo-name . ,test-name)
                  .
                  ,(map 
                    (λ (test-result)
                      (match test-result
                        [`((time ,timings) (input-length ,length) (grammar-size ,size))
                         `(,length . ,(truncate (avg (map 
                                                      (λ (x) 
                                                        #;(- (car x) (cdr x))
                                                        (car x)) 
                                                      timings))))]
                        [`((input-length ,length) (grammar-size ,size))
                         `(,length . too-large)]))
                    test-results))]))
              result-list)]))
     results)))
    (define sizes
      (map
       (λ (algo-results)
         (match algo-results
           [`(,algo-name . ,result-list)
            (map
             (λ (result-set)
               (match result-set
                 [`(,test-name . ,test-results)
                  (match (findf 
                          (λ (row)
                            (match row
                              [`((time ,timings) (input-length ,length) (grammar-size ,size))
                               (and (> length 45) (< length 55))]
                              [`((input-length ,length) (grammar-size ,size))
                               (and (> length 45) (< length 55))]))
                          test-results)
                    [`((time ,timings) (input-length ,length) (grammar-size ,size))
                     `((,algo-name . ,test-name) ,size)]
                    [`((input-length ,length) (grammar-size ,size))
                     `((,algo-name . ,test-name) ,size)])]))
             result-list)]))
       results))
      (list times sizes))

#;(define results (read (open-input-file "results.rktd")))
#;(mangle-results results)