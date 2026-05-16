(define-library (wqy24 math)
 (import (scheme base))
 (export fmod)
 (begin
  (define (fmod a b)
   (- a (* b (truncate (/ a b)))))
  (define (sign a)
   (cond
    [(zero? a) 0]
    [(positive? a) 1]
    [else -1]))))
