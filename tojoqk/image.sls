#!r6rs
(library (tojoqk image)
  (export make width height ref set!)
  (import (except (rnrs) set!))
  
  (define make
    (lambda (w h)
      (let ([vec (make-vector h)])
        (let loop ([i 0])
          (if (= i h)
              vec
              (begin
                (vector-set! vec i (make-vector w 0))
                (loop (+ i 1))))))))

  (define width
    (lambda (img)
      (vector-length (vector-ref img 0))))
  
  (define height
    (lambda (img)
      (vector-length img)))

  (define ref
    (lambda (img x y)
      (vector-ref (vector-ref img y) x)))

  (define set!
    (lambda (img x y v)
      (vector-set! (vector-ref img y) x v))))
