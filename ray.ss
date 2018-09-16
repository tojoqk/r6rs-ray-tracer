#!r6rs
(import (rnrs)
        (tojoqk image pnm)
        (srfi :39 parameters)
        (prefix (tojoqk ray-tracer) ray:)
        (tojoqk benchmark))

(define test
  (lambda ()
    (define rgb->mat
      (lambda (r g b)
        (ray:make-material (ray:make-intensity r g b)
                           (ray:make-intensity 0.2 0.2 0.2) 1
                           (ray:make-intensity 0.0 0.0 0.0))))
    (define red (rgb->mat 1 0 0))
    (define green (rgb->mat 0 1 0))
    (define blue (rgb->mat 0 0 1))
    (define cyan (rgb->mat 0 1 1))
    (define yellow (rgb->mat 1 1 0))
    (define maegnta (rgb->mat 1 0 1))
    (define white (rgb->mat 1 1 1))

    (define film-width 0.036)
    (define film-height 0.024)
    (define focal-distance 0.050)

    (define mirror (ray:make-material
                    (ray:make-intensity 0 0 0)
                    (ray:make-intensity 0 0 0)
                    1
                    (ray:make-intensity 0 0 0)
                    (ray:make-intensity 1 1 1)))

    (define scene
      (ray:make-scene
       (ray:make-camera
        (ray:make-position 0 -1.7 -10)
        (ray:make-position  0 0 0)
        (ray:make-direction 0 -1 0)
        focal-distance
        film-width film-height)
       (list (ray:make-positional-light
              (ray:make-position -0.2 -3.0 -8)
              (ray:make-intensity  1 1 1)))
       (ray:make-intensity 0.00 0.00 0.00)
       (list (ray:make-object
              (ray:make-sphere (ray:make-position 0 -0.5 0)
                               0.5)
              mirror)
             (ray:make-object
              (ray:make-sphere (ray:make-position  -1 -0.5 0)
                               0.25)
              red)
             (ray:make-object
              (ray:make-sphere (ray:make-position  1 -0.5 0)
                               0.25)
              blue)
             (ray:make-object
              (ray:make-sphere (ray:make-position  0 -0.5 -1)
                               0.25)
              green)
             (ray:make-object
              (ray:make-sphere (ray:make-position  0 -0.5 -1)
                               0.25)
              yellow)

             (ray:make-object
              (ray:make-triangle
               (ray:make-position 0.0 -2.0 0.0)
               (ray:make-position -1.0 -1.0 0.0)
               (ray:make-position 1.0 -1.0 0.0))
              cyan)

             (ray:make-object
              (ray:make-plane (ray:make-position -5 0 0)
                              (ray:make-direction 1 0 0))
              red)

             (ray:make-object
              (ray:make-plane (ray:make-position 5 0 0)
                              (ray:make-direction -1 0 0))
              blue)
             (ray:make-object
              (ray:make-plane (ray:make-position 0 4 0)
                              (ray:make-direction 0 -1 0))
              green)
             (ray:make-object
              (ray:make-plane (ray:make-position 0 -20 0)
                          (ray:make-direction 0 1 0))
              yellow)
             (ray:make-object
              (ray:make-plane (ray:make-position 0 0 20)
                          (ray:make-direction 0 0 -1))
              white)
             (ray:make-object
              (ray:make-plane (ray:make-position 0 0 -20)
                          (ray:make-direction 0 0 1))
              white))
       1))

    (save-ppm (ray:shoot 640
                         (exact (div (* 640 film-height) film-width))
                         scene)
              "test1.ppm")))

(define test2
  (lambda ()
    (define rgb->mat
      (lambda (r g b)
        (ray:make-material (ray:make-intensity r g b)
                           (ray:make-intensity 0.0 0.0 0.0) 1
                           (ray:make-intensity 0.0 0.0 0.0))))

    (define red (rgb->mat 1 0 0))
    (define green (rgb->mat 0 1 0))
    (define white (rgb->mat 1 1 1))

    (define mirror (ray:make-material
                    (ray:make-intensity 0 0 0)
                    (ray:make-intensity 0 0 0)
                    1
                    (ray:make-intensity 0 0 0)
                    (ray:make-intensity 1 1 1)))

    (define sukesuke (ray:make-material
                      (ray:make-intensity 0 0 0)
                      (ray:make-intensity 0 0 0)
                      1
                      (ray:make-intensity 0 0 0)
                      (ray:make-intensity 1 1 1)
                      1.51))

    (define sphere1 (ray:make-object
                     (ray:make-sphere
                      (ray:make-position -0.4 0.65 3)
                      0.35)
                     mirror))

    (define sphere2 (ray:make-object
                     (ray:make-sphere (ray:make-position 0.5 0.65 2)
                                      0.35)
                     sukesuke))

    (define sphere3 (ray:make-object
                     (ray:make-sphere (ray:make-position 0.0 0.0 2)

                                      0.35)
                     red))

    (define plane1 (ray:make-object
                    (ray:make-plane
                     (ray:make-position 0 -1 0)
                     (ray:make-direction 0 1 0))
                    white))

    (define plane2 (ray:make-object
                    (ray:make-plane
                     (ray:make-position 0 -1 0)
                     (ray:make-direction 0 1 0))
                    white))

    (define plane3 (ray:make-object
                    (ray:make-plane (ray:make-position 1 0 0)
                                    (ray:make-direction -1 0 0))
                    green))

    (define plane4 (ray:make-object
                    (ray:make-plane (ray:make-position -1 0 0)
                                    (ray:make-direction 1 0 0))
                    red))

    (define plane5 (ray:make-object
                    (ray:make-plane (ray:make-position 0 0 5)
                                    (ray:make-direction 0 0 -1))
                    white))

    (define film-width 2)
    (define film-height 2)
    (define focal-distance 5)

    (define camera
      (ray:make-camera
       (ray:make-position 0 0 -5)
       (ray:make-position 0 0 0)
       (ray:make-direction 0 -1 0)
       focal-distance
       film-width film-height))

    (define light1
      (ray:make-positional-light
       (ray:make-position 0 -0.9 2.5)
       (ray:make-intensity 1.0 1.0 1.0)))

    (define scene
      (ray:make-scene
       camera
       (list light1)
       (ray:make-intensity 0.00 0.00 0.00)
       (list sphere1 sphere2 sphere3
             plane1 plane2 plane3 plane4 plane5)
       1))

    (save-ppm (ray:shoot 512
                         (exact (div (* 512 film-height) film-width))
                         scene)
              "test2.ppm")))

(run-benchmark 'test 1 test)
(run-benchmark 'test2 1 test2)
