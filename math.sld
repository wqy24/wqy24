(define-library (wqy24 math)
 (import (scheme base))
 (export fmod)
 (begin
  (define (fmod a b)
   (- a (* b (truncate (/ a b)))))))
