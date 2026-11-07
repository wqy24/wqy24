(define-library (wqy24 case*)
 (import (except (scheme base) member) (only (srfi 1) member))
 (export case*)
 (begin
  (define-syntax case*
   (syntax-rules ()
    [(_ proc e [l code ...] ...)
     (let [[tmp e]]
      (cond
       [(member tmp 'l proc) code ...] ...))]))))
