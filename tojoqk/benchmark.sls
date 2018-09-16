#!r6rs
(library (tojoqk benchmark)
  (export run-benchmark)
  (import (rnrs)
          (only (srfi :19 time)
                current-time time-difference add-duration!
                make-time time-second time-nanosecond))

  (define duration->number
    (lambda (d)
      (+ (time-second d)
         (/ (time-nanosecond d) (expt 10.0 9)))))

  (define sum
    (lambda (ls)
      (fold-left + 0.0 ls)))

  (define average
    (lambda (ls)
      (/ (sum ls)
         (length ls))))

  (define stdev
    (lambda (ls)
      (let ([avg (average ls)])
        (sqrt (average (map (lambda (x) (expt (- x avg) 2)) ls))))))
      
  (define run-benchmark
    (case-lambda
      [(name count thunk test)
       (let ([dat
              (let f ([i 0] [acc '()])
                (if (= i count)
                    acc
                    (let ([start (current-time)])
                      (let ([result (thunk)])
                        (let ([end (current-time)])
                          (if (test result)
                              (f (+ i 1)
                                 (cons (duration->number
                                        (time-difference end start))
                                       acc))
                              #f))))))])
         (for-each display `(, name ":"))
         (newline)
         (if dat
             (begin
               (for-each display
                         `("  " "times:    " ,(length dat)))
               (newline)
               (for-each display
                         `("  " "average:  " ,(average dat)))
               (newline)
               (for-each display
                         `("  " "stdev:    " ,(stdev dat)))
               (newline))
             (begin
               (for-each display
                         (list "  " "failure!"))
               (newline))))]
      [(name count thunk)
       (run-benchmark name count thunk (lambda (x) #t))])))
