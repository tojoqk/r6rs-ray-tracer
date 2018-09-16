#!r6rs
(library (tojoqk image pnm)
  (export load-pbm load-pgm load-ppm
          save-pbm save-pgm save-ppm)
  (import (rnrs)
          (prefix (tojoqk image) im:)
          (prefix (tojoqk image rgb) rgb:))

  (define-syntax dotimes
    (lambda (x)
      (syntax-case x ()
        [(_ (var n result)
            body body* ...)
         (identifier? #'var)
         #'(let ([t n])
             (do ([var 0 (fx+ var 1)])
                 ((fx=? var t) result)
               body body* ...))]
        [(_ (var n)
            body body* ...)
         (identifier? #'var)
         #'(dotimes (var n (if #f #f))
             body body* ...)])))

  (define LF 10)
  (define CL 13)
  (define tab 9)
  (define space 32)
  (define comment 35)
  (define (newline-u8 port)
    (put-u8 port LF))

  (define (sep? x)
    (or (fx=? x LF)
        (fx=? x CL)
        (fx=? x tab)
        (fx=? x space)))

  (define (comment? x)
    (fx=? x comment))
  (define (newline? x)
    (fx=? x LF))
    
  (define (skip-line in)
    (let loop ()
      (let ([byte (get-u8 in)])
        (cond [(eof-object? byte)
               (loop)]
              [(newline? byte) #t]
              [else
               (loop)]))))

  (define (read-token in)
    (define (skip)
      (let loop ()
        (let ([byte (lookahead-u8 in)])
          (cond ([eof-object? byte] #f)
                [(sep? byte)
                 (get-u8 in)
                 (loop)]
                [(comment? byte)
                 (skip-line in)
                 (loop)]
                [else #f]))))
    (skip)
    (call/cc
     (lambda (exit)
       (call-with-string-output-port
         (lambda (out)
           (let loop ()
             (let [(b (lookahead-u8 in))]
               (cond [(eof-object? b) (exit b)]
                     [(sep? b) #f]
                     [(comment? b)
                      (skip-line in)
                      (loop)]
                     [else
                      (write-char (integer->char (get-u8 in)) out)
                      (loop)]))))))))

  (define (load-pbm filename)
    (call-with-port (open-file-input-port filename)
      (lambda (port)
        (read-pbm port))))
    
  (define (load-pgm filename)
    (call-with-port (open-file-input-port filename)
      (lambda (port)
        (read-pgm port))))

  (define (load-ppm filename)
    (call-with-port (open-file-input-port filename)
      (lambda (port)
        (read-ppm port))))

  (define (read-pbm port)
    (let ([type (read-token port)])
      (cond [(string=? type "P1")
             (read-p1 port)]
            [(string=? type "P4")
             (read-p4 port)]
            [else
             (error 'read-pbm "invalid-pbm-file")])))
  
  (define (read-pgm port)
    (let ([type (read-token port)])
      (cond [(string=? type "P2")
             (read-p2 port)]
            [(string=? type "P5")
             (read-p5 port)]
            [else
             (error 'read-pgm "invalid-pgm-file")])))

  (define (read-ppm port)
    (let ([type (read-token port)])
      (cond [(string=? type "p3")
             (read-p3 port)]
            [(string=? type "P6")
             (read-p6 port)]
            [else
             (error 'read-ppm "invalid-ppm-file")])))

  (define (read-p1 port)
    (let* ([width (string->number (read-token port))]
           [height (string->number (read-token port))]
           [n (fx* height width)])
      (let* ([img (im:make width height)])
        (vector-for-each
         (lambda (row)
           (dotimes (i width)
             (let ([x (string->number (read-token port))])
               (vector-set! row i x))))
         img)
        img)))
  
  (define (read-p2 port)
    (let* ([width (string->number (read-token port))]
           [height (string->number (read-token port))]
           [maxval (string->number (read-token port))]
           [n (* width height)])
      (unless (= maxval 255)
        (assertion-violation
         'read-pgm "not implemented maxval" maxval))
      (let ([img (im:make width height)])
        (vector-for-each
         (lambda (row)
           (dotimes (j width)
             (let ([x (string->number (read-token port))])
               (vector-set! row j x))))
         img)
        img)))

  (define (read-p3 port)
    (let* ([width (string->number (read-token port))]
           [height (string->number (read-token port))]
           [n (* width height)]
           [maxval (string->number (read-token port))])
      (unless (= maxval 255)
        (assertion-violation
         'read-pgm "not implemented maxval" maxval))
      (let ([img (im:make width height)])
        (vector-for-each
         (lambda (row)
           (dotimes (i width)
             (let* ([r (string->number (read-token port))]
                    [g (string->number (read-token port))]
                    [b (string->number (read-token port))])
               (vector-set! img i (rgb:make r g b)))))
         img)
        img)))

  (define (read-p4 port)
    (let* ([width (string->number (read-token port))]
           [height (string->number (read-token port))]
           [cols (fxdiv width 8)]
           [rest (fxmod width 8)])
      (let ([img (im:make width height)])
        (get-u8 port)
        (vector-for-each
         (lambda (row)
           (dotimes (c cols)
             (let ([byte (get-u8 port)])
               (do ([i 7 (fx- i 1)]
                    [k (* c 8) (+ k 1)])
                   ((fx=? i -1))
                 (vector-set!
                  row k
                  (if (fxbit-set? byte i) 1 0)))))
           (unless (fxzero? rest)
             (let ([byte (get-u8 port)])
               (do ([r 0 (+ r 1)]
                    [k (* cols 8) (+ k 1)])
                   ((= r rest))
                 (vector-set!
                  row k
                  (if (fxbit-set? byte (fx- 7 r))
                      1 0))))))
         img)
        img)))
  
  (define (read-p5 port)
    (let* ([width (string->number (read-token port))]
           [height (string->number (read-token port))]
           [maxval (string->number (read-token port))]
           [n (* width height)])
      (get-u8 port)
      (let ([img (im:make width height)])
        (vector-for-each
         (lambda (row)
           (dotimes (i width)
             (vector-set! row i (get-u8 port))))
         img)
        img)))

  (define (read-p6 port)
    (let* ([width (string->number (read-token port))]
           [height (string->number (read-token port))]
           [n (* width height)]
           [maxval (string->number (read-token port))])
      (get-u8 port)
      (let ([img (im:make width height)])
        (vector-for-each
         (lambda (row)
           (dotimes (i width)
             (let* ([r (get-u8 port)]
                    [g (get-u8 port)]
                    [b (get-u8 port)])
               (vector-set! row i (rgb:make r g b)))))
         img)
        img)))

  (define-syntax bits->byte
    (syntax-rules ()
      [(_ b7 b6 b5 b4 b3 b2 b1 b0)
       (fxior
        (fxarithmetic-shift b7 7)
        (fxarithmetic-shift b6 6)
        (fxarithmetic-shift b5 5)
        (fxarithmetic-shift b4 4)
        (fxarithmetic-shift b3 3)
        (fxarithmetic-shift b2 2)
        (fxarithmetic-shift b1 1)
        b0)]))
    
  (define (write-p4 img port)
    (let* ([height (im:height img)]
           [width (im:width img)]
           [rest (mod width 8)]
           [cols (div width 8)])
      (let-syntax
          ([put8
            (syntax-rules ()
              [(_ vec i)
               (put-u8
                port
                (bits->byte (vector-ref vec i)
                            (vector-ref vec (fx+ i 1))
                            (vector-ref vec (fx+ i 2))
                            (vector-ref vec (fx+ i 3))
                            (vector-ref vec (fx+ i 4))
                            (vector-ref vec (fx+ i 5))
                            (vector-ref vec (fx+ i 6))
                            (vector-ref vec (fx+ i 7))))])])
        (put-bytevector port (string->utf8 "P4"))
        (newline-u8 port)
        (put-bytevector port
                        (string->utf8 (number->string width)))
        (put-u8 port space)
        (put-bytevector
         port
         (string->utf8 (number->string height)))
        (newline-u8 port)
        (if (fxzero? rest)
            (begin
              (vector-for-each
               (lambda (row)
                 (dotimes (c cols)
                   (let ((i (fx* 8 c)))
                     (put8 row i))))
               img))
            (vector-for-each
             (lambda (row)
               (dotimes (c cols)
                 (let ((i (fx* 8 c)))
                   (put8 row i)))
               (put-u8
                port
                (let loop ([i 0] [r rest] [acc 0])
                  (let ([x (* 8 cols)])
                    (if (zero? r)
                        acc
                        (loop (fx+ i 1) (fx- r 1)
                              (fxior acc
                                     (fxarithmetic-shift
                                      (vector-ref row (+ x i))
                                      (fx- 7 i)))))))))
             img)))))
    
  (define (write-p5 img port)
    (let* ([height (im:height img)]
           [width (im:width img)])
      (put-bytevector port (string->utf8 "P5"))
      (newline-u8 port)
      (put-bytevector
       port
       (string->utf8 (number->string width)))
      (put-u8 port space)
      (put-bytevector
       port
       (string->utf8 (number->string height)))
      (newline-u8 port)
      (put-bytevector
       port (string->utf8 (number->string 255)))
      (newline-u8 port)
      (vector-for-each
       (lambda (row)
         (dotimes (i width)
           (put-u8 port (vector-ref row i))))
       img)))
    
  (define (write-p6 img port)
    (let ([height (im:height img)]
          [width (im:width img)])
      (put-bytevector port (string->utf8 "P6"))
      (newline-u8 port)
      (put-bytevector
       port
       (string->utf8 (number->string width)))
      (put-u8 port space)
      (put-bytevector
       port
       (string->utf8 (number->string height)))
      (newline-u8 port)
      (put-bytevector
       port
       (string->utf8 (number->string 255)))
      (newline-u8 port)
      (vector-for-each
       (lambda (row)
         (dotimes (i width)
           (let ([c (vector-ref row i)])
             (put-u8 port (rgb:red c))
             (put-u8 port (rgb:green c))
             (put-u8 port (rgb:blue c)))))
       img)))
  
  (define save-pbm
    (case-lambda
      [(image filename opts)
       (call-with-port (open-file-output-port filename opts)
         (lambda (port) (write-p4 image port)))]
      [(image filename)
       (save-pbm image filename (file-options no-fail))]))
  
  (define save-pgm
    (case-lambda
      [(image filename opts)
       (call-with-port (open-file-output-port filename opts)
         (lambda (port) (write-p5 image port)))]
      [(image filename)
       (save-pgm image filename (file-options no-fail))]))
  
  (define save-ppm
    (case-lambda
      [(image filename opts)
       (call-with-port (open-file-output-port filename opts)
         (lambda (port) (write-p6 image port)))]
      [(image filename)
       (save-ppm image filename (file-options no-fail))])))
