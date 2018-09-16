#!r6rs
(library (tokyo tojo syntax splicing)
  (export ;; with-splicing
          with-splicing*
          ;; with-splicing-values
          with-splicing-values*)
  (import (rnrs))

  (define-syntax with-splicing
    (lambda (x)
      (syntax-case x ()
        [(_ var (x ...) body body* ...)
         (with-syntax
             ([tr
               (let f ([tr #'(begin body body* ...)])
                 (syntax-case tr ()
                   [(v . p)
                    (and (identifier? #'v)
                         (eq? (syntax->datum #'var)
                              (syntax->datum #'v)))
                    (f #'(x ... . p))]
                   [(p1 . p2)
                    #`(#,(f #'p1) . #,(f #'p2))]
                   [tr #'tr]))])
           #'tr)])))
  
  (define-syntax with-splicing*
    (syntax-rules ()
      [(_ () body body* ...)
       (begin body body* ...)]
      [(_ ([v e] [v* e*] ...) body body* ...)
       (with-splicing v e
         (with-splicing* ([v* e*] ...)
           body body* ...))]))


  (define-syntax with-splicing-values/n
    (lambda (x)
      (syntax-case x ()
        [(_ () gs n expr body body* ...)
         #'(let-values ([gs expr])
             body body* ...)]
        [(_ (v1 v2 ...) (g ...) n expr body body* ...)
         (with-syntax ([gs
                        (generate-temporaries (let loop ([i (syntax->datum #'n)])
                                                (if (zero? i)
                                                    '()
                                                    (cons '() (loop (- i 1))))))])
           #'(with-splicing v1 gs
               (with-splicing-values/n (v2 ...) (g ... . gs) n expr
                   body body* ...)))])))

  (define-syntax with-splicing-values
    (syntax-rules ()
      [(_ (v1 v2 ...) n expr body body* ...)
       (with-splicing-values/n (v1 v2 ...) () n expr body body* ...)]
      [(_ var n expr body body* ...)
       (with-splicing-values (var) n expr body body* ...)]))
  
  (define-syntax with-splicing-values*
    (syntax-rules ()
      [(_ ([v1 n1 e1] [v2 n2 e2] ...) body body* ...)
       (with-splicing-values v1 n1 e1
         (with-splicing-values* ([v2 n2 e2] ...)
           body body* ...))]
      [(_ () body body* ...)
       (begin body body* ...)]))
  )
