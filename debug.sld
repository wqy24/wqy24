(define-library (wqy24 debug)
 (import (scheme base) (scheme case-lambda) (scheme write))
 (export debug default-log-port)
 (begin
  (define default-log-port
   (make-parameter (current-output-port)
    (lambda (p) p)))
  (define debug
   (case-lambda
    [(msg obj)
     (debug msg obj (default-log-port))]
    [(msg obj p)
     (display "[DEBUG] ")
     (display msg p)
     (write-char #\: p)
     (write-shared obj p)
     (newline)
     obj]))))
