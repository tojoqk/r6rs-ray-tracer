#!r6rs
(library (tokyo tojo image rgb)
  (export make rgb red green blue)
  (import (rnrs))
  
  (define make
    (case-lambda
      [(r g b)
       (bitwise-ior (bitwise-arithmetic-shift r 16)
                    (bitwise-arithmetic-shift g 8)
                    b)]
      [(k)
       (make k k k)]))
  
  (define rgb
    (lambda (c)
      (values (red c) (green c) (blue c))))
  
  (define red
    (lambda (c)
      (bitwise-and (bitwise-arithmetic-shift c -16)
                   255)))
  (define green
    (lambda (c)
      (bitwise-and (bitwise-arithmetic-shift c -8)
                   255)))
  (define blue
    (lambda (c)
      (bitwise-and c 255))))
