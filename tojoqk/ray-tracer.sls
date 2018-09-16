#!r6rs
(library (tojoqk ray-tracer)
  (export
   make-positional-light
   make-directional-light
   make-object
   make-sphere
   make-plane
   make-triangle
   make-material
   make-camera
   make-scene

   make-direction
   make-position
   make-intensity

   reflection-count scale background
   shoot shoot!)
  (import (rnrs)
          (srfi :39 parameters)
          (tojoqk syntax splicing)
          (prefix (tojoqk image) im:)
          (prefix (tojoqk image rgb) rgb:))

  (define-record-type (stack %make-stack stack?)
    (fields (mutable pos stack-pos set-pos!)
            (immutable vec stack-vec)
            (immutable n stack-length)))

  (define-syntax with-splicing
    (syntax-rules ()
      [(_ var (x ...) body body* ...)
       (with-splicing* ([var (x ...)])
         body body* ...)]))
  
  (define-syntax with-vec/n
    (lambda (x)
      (syntax-case x ()
        [(_ () gs expr body body* ...)
         #'(let-values ([gs expr])
             body body* ...)]
        [(_ (v1 v2 ...) (g ...) expr body body* ...)
         (with-syntax ([(gx gy gz)
                        (generate-temporaries '(gx gy gz))])
           #'(with-splicing v1 (gx gy gz)
               (with-vec/n (v2 ...) (g ... gx gy gz) expr
                   body body* ...)))])))
  
  (define-syntax with-vec
    (syntax-rules ()
      [(_ (v1 v2 ...) expr body body* ...)
       (with-vec/n (v1 v2 ...) () expr body body* ...)]
      [(_ var expr body body* ...)
       (with-vec (var) expr body body* ...)]))
  
  (define-syntax with-vecs*
    (syntax-rules ()
      [(_ ([v1 e1] [v2 e2] ...) body body* ...)
       (with-vec v1 e1
         (with-vecs* ([v2 e2] ...)
             body body* ...))]
      [(_ () body body* ...)
       (begin body body* ...)]))
  
  (with-splicing* ([v (x y z)]
                   [v1 (x1 y1 z1)]
                   [v2 (x2 y2 z2)])
    (define add
      (lambda (v1 v2)
        (values (+ x1 x2) (+ y1 y2) (+ z1 z2))))
    
    (define subtract
      (lambda (v1 v2)
        (values (- x1 x2) (- y1 y2) (- z1 z2))))
    
    (define negate
      (lambda (v)
        (values (- x) (- y) (- z))))
    
    (define dot
      (lambda (v1 v2)
        (+ (* x1 x2) (* y1 y2) (* z1 z2))))
    
    (define mag
      (lambda (v)
        (sqrt (dot v v))))
    
    (define cross
      (lambda (v1 v2)
        (values
         (- (* y1 z2) (* z1 y2))
         (- (* z1 x2) (* x1 z1))
         (- (* x1 y2) (* y1 x2)))))
    
    (define scalar
      (lambda (s v)
        (values (* s x) (* s y) (* s z))))
    
    (define normalize
      (lambda (v)
        (scalar (/ (mag v)) v))))
  
  (define-record-type (light %make-light light?)
    (opaque #t))

  (with-splicing* ([p (p-x p-y p-z)])
    (define light-direction
      (lambda (x p)
        (cond [(positional-light? x)
               (with-vecs* ([sp (positional-light-position x)]
                            [d* (subtract sp p)]
                            [d (normalize d*)])
                 (values d))]
              [(directional-light? x)
               (directional-light-direction x)]
              [else (error 'light-direction "not light source"
                           x)]))))

  (define light-distance
    (with-splicing p (x y z)
      (lambda (sh p)
        (cond [(positional-light? sh)
               (with-vecs* ([sp (positional-light-position sh)]
                            [sp-p (subtract sp p)])
                 (mag sp-p))]
              [(directional-light? sh)
               +inf.0]
              [else
               (error 'light-distance "not light source"
                      x)]))))

  (define light-intensity
    (lambda (x)
      (cond [(positional-light? x)
             (positional-light-intensity x)]
            [(directional-light? x)
             (directional-light-intensity x)]
            [else (error 'light-intensity "not light source"
                         x)])))

  (define-record-type (positional-light
                       %make-positional-light
                       positional-light?)
    (parent light)
    (fields (immutable direction %positional-light-position)
            (immutable int %positional-light-intensity))
    (opaque #t)
    (sealed #t))
  
  (define make-positional-light
    (lambda (p i)
      (cond [(not (position? p))
             (error 'make-positional-light "not position" p)]
            [(not (intensity? i))
             (error 'make-positional-light "not intensity" i)])
      (%make-positional-light p i)))

  (define positional-light-position
    (lambda (s)
      (position->values (%positional-light-position s))))

  (define positional-light-intensity
    (lambda (s)
      (intensity->values (%positional-light-intensity s))))

  (define-record-type (directional-light
                       %make-directional-light
                       directional-light?)
    (parent light)
    (fields (immutable direction %directional-light-direction)
            (immutable int %directional-light-intensity))
    (opaque #t)
    (sealed #t))
  
  (define make-directional-light
    (lambda (d i)
      (cond [(not (direction? d))
             (error 'make-directional-light "not direction" d)]
            [(not (intensity? i))
             (error 'make-directional-light "not intensity" i)])
      (%make-directional-light d i)))

  (define directional-light-direction
    (lambda (s)
      (direction->values (%directional-light-direction s))))

  (define directional-light-intensity
    (lambda (s)
      (intensity->values (%directional-light-intensity s))))

  (define-record-type (object %make-object object?)
    (fields (immutable shape object-shape)
            (immutable material object-material))
    (opaque #t)
    (sealed #t))

  (define make-object
    (lambda (shape material)
      (cond [(not (shape? shape))
             (error 'make-object "not shape" shape)]
            [(not (material? material))
             (error 'make-object "not material" material)])
      (%make-object shape material)))

  (define-record-type (shape %make-shape shape?)
    (opaque #t))

  (define intersect
    (lambda (px py pz dx dy dz sh)
      (cond
       [(sphere? sh)
        (sphere-intersect px py pz dx dy dz sh)]
       [(plane? sh)
        (plane-intersect px py pz dx dy dz sh)]
       [(triangle? sh)
        (triangle-intersect px py pz dx dy dz sh)]
       [else
        (error 'intersect "not shape" sh)])))

  (define-record-type (sphere %make-sphere sphere?)
    (parent shape)
    (fields (immutable center %sphere-center)
            (immutable radius sphere-radius))
    (sealed #t)
    (opaque #t))
  
  (define make-sphere
    (lambda (c r)
      (cond [(not (position? c))
             (error 'make-sphere "not position" c)]
            [(not (and (real? r) (<= 0 r)))
             (error 'make-sphere "not radius" r)])
      (%make-sphere c r)))

  (define sphere-center
    (lambda (s)
      (position->values (%sphere-center s))))

  (define sphere-intersect
    (lambda (px py pz dx dy dz sh)
      (with-vecs* ([p (values px py pz)]
                   [d (values dx dy dz)]
                   [center (sphere-center sh)]
                   [s (subtract p center)])
        (let ([r (sphere-radius sh)])
          (let ([a (dot d d)]
                [b (* 2.0 (dot s d))]
                [c (- (dot s s) (* r r))])
            (let ([det (- (* b b) (* 4.0 a c))])
              (if (not (negative? det))
                  (let ([t1 (/ (+ (- b) (sqrt det))
                               (* 2 a))]
                        [t2 (/ (- (- b) (sqrt det))
                               (* 2 a))])
                    (if (and (negative? t1)
                             (negative? t2))
                        (values)
                        (let ([t (cond [(negative? t1) t2]
                                       [(negative? t2) t1]
                                       [else (min t1 t2)])])
                          (with-vecs* ([td (scalar t d)]
                                       [p+td (add p td)]
                                       [n* (subtract p+td center)]
                                       [n (normalize n*)])
                            (values t p+td n)))))
                  (values))))))))


  (define-record-type (plane %make-plane plane?)
    (parent shape)
    (fields (immutable position %plane-position)
            (immutable normal %plane-normal))
    (sealed #t)
    (opaque #t))

  (define make-plane
    (lambda (p n)
      (cond [(not (position? p))
             (error 'make-plane "not position" p)]
            [(not (direction? n))
             (error 'make-plane "not direction" n)])
      (%make-plane p n)))
  
  (define plane-position
    (lambda (p)
      (position->values (%plane-position p))))
  
  (define plane-normal
    (lambda (p)
      (direction->values (%plane-normal p))))
  
  (define plane-intersect
    (lambda (px py pz dx dy dz sh)
      (with-vecs* ([p (values px py pz)]
                   [d (values dx dy dz)]
                   [pos (plane-position sh)]
                   [s (subtract p pos)]
                   [n (plane-normal sh)])
        (let ([d-dot-n (dot d n)])
          (if (not (zero? d-dot-n))
              (let ([t (- (/ (dot s n)
                             d-dot-n))])
                (if (positive? t)
                    (with-vecs* ([td (scalar t d)]
                                 [p+td (add p td)])
                      (values t p+td n))
                    (values)))
              (values))))))

  (define-record-type (triangle %make-triangle triangle?)
    (parent shape)
    (fields (immutable p1 %triangle-p1)
            (immutable p2 %triangle-p2)
            (immutable p3 %triangle-p3)
            (immutable side1 %triangle-side1)
            (immutable side2 %triangle-side2)
            (immutable normal %triangle-normal))
    (sealed #t)
    (opaque #t))
  
  (define make-triangle
    (lambda (p1 p2 p3)
      (cond [(not (position? p1))
             (error 'make-triangle "not position" p1)]
            [(not (position? p2))
             (error 'make-triangle "not position" p2)]
            [(not (position? p3))
             (error 'make-triangle "not position" p3)])
      (with-vecs* ([p1* (position->values p1)]
                   [p2* (position->values p2)]
                   [p3* (position->values p3)]
                   [side1 (subtract p2* p1*)]
                   [side2 (subtract p3* p1*)]
                   [n* (cross side1 side2)])
        (%make-triangle
         p1 p2 p3
         (vector side1) (vector side2)
         (make-direction n*)))))

  (define triangle-p1
    (lambda (t)
      (position->values (%triangle-p1 t))))
  (define triangle-p2
    (lambda (t)
      (position->values (%triangle-p2 t))))
  (define triangle-p3
    (lambda (t)
      (position->values (%triangle-p3 t))))

  (define triangle-side1
    (lambda (t)
      (vector3->values (%triangle-side1 t))))
  
  (define triangle-side2
    (lambda (t)
      (vector3->values (%triangle-side2 t))))
  
  (define triangle-normal
    (lambda (t)
      (direction->values (%triangle-normal t))))

  (define triangle-intersect
    ;; Tomas Moller
    (with-splicing* ([p (px py pz)]
                     [d (dx dy dz)])
      (lambda (p d sh)
        (define det
          (with-splicing* ([a (ax ay az)]
                           [b (bx by bz)]
                           [c (cx cy cz)])
            (lambda (a b c)
              (- (+ (* ax by cz)
                    (* ay bz cx)
                    (* az bx cy))
                 (+ (* ax bz cy)
                    (* ay bx cz)
                    (* az by cx))))))
        (with-vecs* ([n (triangle-normal sh)]
                     [p1 (triangle-p1 sh)]
                     [p2 (triangle-p2 sh)]
                     [p3 (triangle-p3 sh)]
                     [side1 (triangle-side1 sh)]
                     [side2 (triangle-side2 sh)]
                     [p-p1 (subtract p p1)]
                     [minus-d (scalar -1.0 d)])
          (let ([denom (det side1 side2 minus-d)])
            (let ([u (/ (det p-p1 side2 minus-d)
                        denom)])
              (if (<= 0 u 1)
                  (let ([v (/ (det side1 p-p1 minus-d)
                              denom)])
                    (if (and (<= 0 v)
                             (<= (+ u v) 1))
                        (let ([t (/ (det side1 side2 p-p1)
                                    denom)])
                          (with-vecs* ([td (scalar t d)]
                                       [p+td (add p td)])
                            (if (< 0 t)
                                (values t p+td n)
                                (values))))
                        (values)))
                  (values))))))))
  
  (define-record-type (material %make-material material?)
    (fields (immutable diffuse %material-diffuse)
            (immutable specular %material-specular)
            (immutable shiness material-shininess)
            (immutable ambient %material-ambient)
            (immutable perfect-reflectance-flag
                       material-perfect-reflectance?)
            (immutable reflect %material-reflect)
            (immutable reflaction-flag material-refractance?)
            (immutable reflaction-index
                       material-reflaction-index))
    (opaque #t)
    (sealed #t))
  
  (define make-material
    (case-lambda
      [(d s alpha a c r)
       (cond [(not (intensity? d))
              (error 'make-material "not intensity" d)]
             [(not (intensity? s))
              (error 'make-material "not intensity" s)]
             [(not (and (real? alpha) (<= 1 alpha)))
              (error 'make-material "not real or not  1 <= alpha"
                     alpha)]
             [(not (intensity? a))
              (error 'make-material "not intensity" a)]
             [(and c (not (intensity? c)))
              (error 'make-material "not intensity" c)]
             [(and (not c) r)
              (error 'make-material "must be perfect-reflectance")]
             [(and r (not (and (real? r) (positive? r))))
              (error 'make-material "not positive real" r)])
       (%make-material d
                       s
                       alpha
                       a
                       (if c #t #f)
                       (if c c #f)
                       (if r #t #f)
                       (if r (inexact r) #f))]
      [(d s alpha a c)
       (make-material d s alpha a c #f)]
      [(d s alpha a)
       (make-material d s alpha a #f)]))

  (define material-diffuse
    (lambda (m)
      (let ([d (%material-diffuse m)])
        (intensity->values d))))
  
  (define material-specular
    (lambda (m)
      (intensity->values (%material-specular m))))

  (define material-ambient
    (lambda (m)
      (intensity->values (%material-ambient m))))

  (define material-reflect
    (lambda (m)
      (intensity->values (%material-reflect m))))

  (define-record-type (camera %make-camera camera?)
    (fields (immutable position %camera-position)
            (immutable target-position %camera-target)
            (immutable up-direction %camera-up)
            (immutable focal-distance camera-focal-distance)
            (immutable width camera-film-width)
            (immutable height camera-film-height))
    (opaque #t)
    (sealed #t))

  (define make-camera
    (lambda (pos target up focal-distnace film-width film-height)
      (cond
       [(not (and (real? film-width) (positive? film-width)))
        (error 'make-camera "not positive-real" film-width)]
       [(not (and (real? film-height) (positive? film-height)))
        (error 'make-camera "not positive-real" film-height)]
       [(not (position? pos))
        (error 'make-camera "not position" pos)]
       [(not (position? target))
        (error 'make-camera "not position" target)]
       [(not (direction? up))
        (error 'make-camera "not direction" up)]
       [(not (and (real? focal-distnace)
                  (positive? focal-distnace)))
        (error 'make-camera "not positive-real" focal-distnace)])
      (%make-camera pos target up
                    (inexact focal-distnace)
                    (inexact film-width)
                    (inexact film-height))))

  
  (define camera-position
    (lambda (s)
      (position->values (%camera-position s))))

  (define camera-target
    (lambda (s)
      (position->values (%camera-target s))))
  
  (define camera-up
    (lambda (s)
      (direction->values (%camera-up s))))

  (define-record-type (scene %make-scene scene?)
    (fields (immutable camera scene-camera)
            (immutable lights scene-lights)
            (immutable ambient %scene-ambient)
            (immutable object scene-objects)
            (immutable refraction-index scene-refraction-index))
    (opaque #t)
    (sealed #t))

  (define make-scene
    (lambda (camera lights a objects ref)
      (cond [(not (camera? camera))
             (error 'make-scene "not camera" camera)]
            [(not (for-all light? lights))
             (error 'make-scene "not lights" lights)]
            [(not (intensity? a))
             (error 'make-scene "not intensity" a)]
            [(not (for-all object? objects))
             (error 'make-scene "not objects" objects)]
            [(not (and (real? ref) (positive? ref)))
             (error 'make-scene "not positive real" ref)]
            [else
             (%make-scene camera lights a objects
                          (inexact ref))])))

  (define scene-ambient
    (lambda (s)
      (intensity->values (%scene-ambient s))))

  (define map-screen
    (lambda (width height cam)
      (with-vecs* ([cp (camera-position cam)]
                   [target (camera-target cam)]
                   [zv* (subtract target cp)]
                   [zv (normalize zv*)]
                   [v (scalar (camera-focal-distance cam) zv)]
                   [p (add cp v)]
                   [yv (camera-up cam)]
                   [xv (cross zv yv)])
        (let ([cw (camera-film-width cam)]
              [ch (camera-film-height cam)])
          (let ([xu-mag (/ cw (- width 1.0))]
                [yu-mag (- (/ ch (- height 1.0)))])
            (with-vecs* ([xv/2cw (scalar (/ cw 2) xv)]
                         [yv/2ch (scalar (- (/ ch 2)) yv)]
                         [p-x  (subtract p xv/2cw)]
                         [orig (subtract p-x yv/2ch)]
                         [xu (scalar xu-mag xv)]
                         [yu (scalar yu-mag yv)])
              (values orig xu yu)))))))
  
  (define reflection-count (make-parameter 20))
  (define scale (make-parameter 1.0))
  (define offset (make-parameter 0.0))
  (define background (make-parameter (rgb:make 100 149 237)))
  (define zero-tolerance 0.000001)

  (define int->color
    (lambda (r g b)
      (define chop
        (lambda (x start end)
          (cond [(< x start) start]
                [(> x end) end]
                [else x])))
      (let ([r (chop (exact (floor (* 255 (+ (offset)
                                             (* (scale) r)))))
                     0 255)]
            [g (chop (exact (floor (* 255 (+ (offset)
                                             (* (scale) g)))))
                     0 255)]
            [b (chop (exact (floor (* 255 (+ (offset)
                                             (* (scale) b)))))
                     0 255)])
        (rgb:make r g b))))

  (define shoot
    (lambda (width height scene)
      (let ([img (im:make width height)])
        (shoot! img scene)
        img)))

  (define shoot!
    (lambda (img scene)
      (let ([w (im:width img)]
            [h (im:height img)])
        (with-vec (orig xu yu)
            (map-screen w h (scene-camera scene))
          (do ([y 0 (+ y 1)])
              ((= y h))
            (do ([x 0 (+ x 1)])
                ((= x w))
              (with-vecs* ([x* (scalar x xu)]
                           [y* (scalar y yu)]
                           [o+x* (add orig x*)]
                           [o+x*+y* (add o+x* y*)])
                (with-vecs* ([p (camera-position
                                 (scene-camera scene))]
                             [d* (subtract o+x*+y* p)]
                             [d (normalize d*)])
                  (call-with-values
                      (lambda () (ray scene p d))
                    (case-lambda
                      ((r g b)
                       (im:set! img x y (int->color r g b)))
                      (()
                       (im:set! img x y (background)))))))))))))
  
  (define int
    (lambda (r g b)
      (values r g b)))
  
  (define int+
    (lambda (r1 g1 b1 r2 g2 b2)
      (values (+ r1 r2)
              (+ g1 g2)
              (+ b1 b2))))
  
  (define int*
    (lambda (r1 g1 b1 r2 g2 b2)
      (values (* r1 r2)
              (* g1 g2)
              (* b1 b2))))
  
  (define ambient-light
    (lambda (scene obj)
      (let ([mat (object-material obj)])
        (with-vecs* ([samb (scene-ambient scene)]
                     [mamb (material-ambient mat)])
          (int* samb mamb)))))

  (define diffuse-reflection
    (with-splicing* ([x (xx xy xz)]
                     [n (nx ny nz)]
                     [acc (accr accg accb)])
      (lambda (scene obj x n)
        (with-vec k (material-diffuse (object-material obj))
          (let ([objs (scene-objects scene)])
            (let loop ([srcs (scene-lights scene)]
                       [accr 0.0] [accg 0.0] [accb 0.0])
              (cond [(null? srcs) (int acc)]
                    [(shadow? objs x (car srcs))
                     (loop (cdr srcs) acc)]
                    [else
                     (with-vecs*
                         ([li (light-intensity (car srcs))]
                          [k*li (int* k li)]
                          [l (light-direction (car srcs)
                                              x)])
                       (let ([dot-n-l (dot n l)])
                         (if (< dot-n-l 0)
                             (loop (cdr srcs) acc)
                             (with-vecs*
                                 ([k1 (scalar dot-n-l k*li)]
                                  [ans (int+ acc k1)])
                               (loop (cdr srcs) ans)))))])))))))
  
  (define specular-reflection
    (with-splicing* ([v (vx vy vz)]
                     [x (xx xy xz)]
                     [n (nx ny nz)]
                     [acc (accr accg accb)])
      (lambda (scene obj v x n)
        (let ([alpha (material-shininess (object-material obj))]
              [objs (scene-objects scene)])
          (with-vec k (material-specular (object-material obj))
            (let loop ([srcs (scene-lights scene)]
                       [accr 0.0] [accg 0.0] [accb 0.0])
              (cond
               [(null? srcs)
                (values acc)]
               ;; [(shadow? objs x (car srcs))
               ;;  (loop (cdr srcs) acc)]
               [else
                (with-vec l (light-direction (car srcs) x)
                  (let ([dot-n-l (dot n l)])
                    (if (< dot-n-l 0)
                        (loop (cdr srcs) accr accg accb)
                        (with-vecs*
                            ([i (light-intensity (car srcs))]
                             [k*i (int* k i)]
                             [tmp1 (scalar (* 2 dot-n-l) n)]
                             [r (subtract tmp1 l)])
                          (let ([dot-v-r (dot v r)])
                            (if (< dot-v-r 0)
                                (loop (cdr srcs) acc)
                                (with-vecs*
                                    ([k1 (scalar
                                          (expt dot-v-r alpha)
                                          k*i)]
                                     [ans (int+ acc k1)])
                                  (loop (cdr srcs)
                                        ans))))))))])))))))

  (define reflection
    (with-splicing* ([d (dx dy dz)]
                     [v (vx vy vz)]
                     [p (px py pz)]
                     [n (nx ny nz)])
      (lambda (scene obj d v p n)
        (let ([mat (object-material obj)])
          (cond
           [(or (not (material-perfect-reflectance? mat))
                (zero? (reflection-count)))
            (int 0.0 0.0 0.0)]
           [else
            (let ([dot-v-n (dot v n)])
              (define (perfect-reflection)
                (with-vecs* ([tmp1 (scalar (* 2 dot-v-n) n)]
                             [r (subtract tmp1 v)]
                             [nr (normalize r)]
                             [snr (scalar zero-tolerance nr)]
                             [next-p (add p snr)])
                  (parameterize ([reflection-count
                                  (- (reflection-count) 1)])
                    (ray scene next-p nr))))
              (define (refractance)
                (with-splicing n* (n*-x n*-y n*-z)
                  (let-values
                      ([(eta1 eta2 n*)
                        (if (positive? dot-v-n)
                            (values
                             (scene-refraction-index scene)
                             (material-reflaction-index mat)
                             n)
                            (with-vec minus-n (scalar -1.0 n)
                              (values
                               (material-reflaction-index mat)
                               (scene-refraction-index scene)
                               minus-n)))])
                    (let* ([etar (/ eta2 eta1)]
                           [cos1 (dot v n*)]
                           [cos2 (* (/ eta1 eta2)
                                    (sqrt
                                     (- (expt etar 2)
                                        (- 1 (expt cos1 2)))))]
                           [omega (- (* etar cos2) cos1)]
                           [rho1 (/ (- (* etar cos1) cos2)
                                    (+ (* etar cos1) cos2))]
                           [rho2 (/ (- omega)
                                    (+ (* etar cos2) cos1))]
                           [cr (/ (+ (expt rho1 2) (expt rho2 2))
                                  2.0)]
                           [ct (- 1 cr)])
                      (with-vecs* ([omega*n
                                    (scalar omega n*)]
                                   [d-omega*n (subtract d omega*n)]
                                   [f (scalar (/ eta1 eta2)
                                              d-omega*n)]
                                   [nf (normalize f)]
                                   [snf (scalar zero-tolerance nf)]
                                   [next-p (add p snf)])
                        (parameterize ([reflection-count
                                        (- (reflection-count) 1)])
                          (call-with-values
                              (lambda ()
                                (ray scene next-p nf))
                            (case-lambda
                              [() cr]
                              [(r g b)
                               (values r g b cr ct)]))))))))
              (with-splicing* ([lr (lr-r lr-g lr-b)]
                               [lf (lf-r lf-g lf-b)])
                (call-with-values perfect-reflection
                  (case-lambda
                    [() (int 0.0 0.0 0.0)]
                    [(lr)
                     (with-vec k (material-reflect mat)
                       (if (material-refractance? mat)
                           (call-with-values refractance
                             (case-lambda
                               [(cr)
                                (with-vec cr*lr (scalar cr lr)
                                  (int* k cr*lr))]
                               [(lf cr ct)
                                (with-vecs* ([cr*lr (scalar cr lr)]
                                             [ct*lf (scalar ct lf)]
                                             [lr+lf (add cr*lr
                                                         ct*lf)])
                                  (int* k lr+lf))]))
                           (int* k lr)))]))))])))))
  

  (define shadow?
    (with-splicing* ([xp (xx xy xz)])
      (lambda (objs xp light-src)
        (let ([dst (light-distance light-src xp)])
          (with-vecs* ([ld (light-direction light-src xp)]
                       [nd (normalize ld)]
                       [sd (scalar zero-tolerance nd)]
                       [init (add xp sd)])
            (let find ([objs objs])
              (if (null? objs)
                  #f
                  (call-with-values
                      (lambda ()
                        (intersect init ld (object-shape (car objs))))
                    (with-splicing* ([nxp (nxpx nxpy nxpz)]
                                     [n (nx ny nz)])
                      (case-lambda
                        [() (find (cdr objs))]
                        [(t nxp n)
                         (if (< t dst)
                             #t
                             (find (cdr objs)))]))))))))))
  
  (define ray
    (with-splicing* ([p (px py pz)]
                     [d (dx dy dz)])
      (lambda (scene p d)
        (with-splicing* ([xp (xpx xpy xpz)]
                         [n  (nx ny nz)]
                         [min-xp (min-xpx min-xpy min-xpz)]
                         [min-n (min-nx min-ny min-nz)])
          (call-with-values
              (lambda ()
                (define minimum
                  (lambda (objs min-obj min-t min-xp min-n)
                    (if (null? objs)
                        (values min-obj min-t min-xp min-n)
                        (let* ([obj (car objs)])
                          (call-with-values
                              (lambda ()
                                (intersect p d (object-shape obj)))
                            (case-lambda
                              [(t xp n)
                               (if (or (not min-t) (< t min-t))
                                   (minimum (cdr objs) obj t xp n)
                                   (minimum (cdr objs)
                                            min-obj
                                            min-t min-xp min-n))]
                              [()
                               (minimum (cdr objs)
                                        min-obj
                                        min-t min-xp min-n)]))))))
                (minimum (scene-objects scene)
                         #f #f
                         #f #f #f  #f #f #f))
            (lambda (obj t xp n)
              (if obj
                  (with-vecs*
                      ([ambient (ambient-light scene obj)]
                       [diffuse (diffuse-reflection scene obj xp n)]
                       [v (scalar -1.0 d)]
                       [specular
                        (specular-reflection scene obj v xp n)]
                       [ref
                        (reflection scene obj d v xp n)]
                       [k1 (int+ ambient diffuse)]
                       [k2 (int+ k1 specular)]
                       [ans (int+ k2 ref)])
                    (values ans))
                  (values))))))))

  (define-record-type (direction %make-direction direction?)
    (fields (immutable x dir-x)
            (immutable y dir-y)
            (immutable z dir-z))
    (opaque #t)
    (sealed #t))

  (define make-direction
    (lambda (x y z)
      (with-vec d (normalize x y z)
        (%make-direction d))))

  (define direction->values
    (lambda (d)
      (values (dir-x d) (dir-y d) (dir-z d))))

  (define-record-type (position %make-position position?)
    (fields (immutable x pos-x)
            (immutable y pos-y)
            (immutable z pos-z))
    (opaque #t)
    (sealed #t))

  (define make-position
    (lambda (x y z)
      (%make-position (inexact x) (inexact y) (inexact z))))

  (define position->values
    (lambda (p) (values (pos-x p) (pos-y p) (pos-z p))))  

  (define-record-type (intensity %make-intensity intensity?)
    (fields (immutable r int-r)
            (immutable g int-g)
            (immutable b int-b))
    (opaque #t)
    (sealed #t))

  (define make-intensity
    (lambda (r g b)
      (%make-intensity (inexact r)
                       (inexact g)
                       (inexact b))))

  (define intensity->values
    (lambda (i)
      (values (int-r i) (int-g i) (int-b i))))
  
  (define vector3->values
    (lambda (v)
      (values (vector-ref v 0)
              (vector-ref v 1)
              (vector-ref v 2)))))
